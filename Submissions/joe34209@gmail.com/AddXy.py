import pandas as pd
import numpy as np
import seaborn as sns
from tqdm import tqdm

import matplotlib.pyplot as plt
import matplotlib.patches as patches
pd.set_option('max_columns', 1000)

from sklearn.neighbors import BallTree

from IPython.core.display import HTML
import time
import math
from scipy.spatial import Voronoi, voronoi_plot_2d
from datetime import datetime
from IPython.display import HTML
import scipy.stats as stats
import matplotlib as mpl
from matplotlib import animation, rc
from matplotlib.patches import Rectangle, Arrow
import plotly.graph_objects as go
import plotly.figure_factory as ff

import glob
import os

np.set_printoptions(suppress=True)

import gc
import math

import pandas as pd
import numpy as np
from catboost import CatBoostRegressor
from sklearn.preprocessing import StandardScaler
import random

import pickle

def convert_orientation(x):
    return (x)%360

def deg_to_rad(deg):
        return deg*np.pi/180

def get_dx_dy(radian_angle, dist):
    dx = dist * math.cos(radian_angle)
    dy = dist * math.sin(radian_angle)
    return dx, dy

def Standardize(W):
        print("Standardizing Data..")
        W['Dir_rad'] = np.mod(90 - W.dir, 360) * math.pi/180.0
        W['ToLeft'] = W.playDirection == "left"
        W['TeamOnOffense'] = "home"
        W.loc[W.possessionTeam != W.PlayerTeam, 'TeamOnOffense'] = "away"
        W['IsOnOffense'] = W.PlayerTeam == W.TeamOnOffense
        W['IsOnOffense'] = W.OnOffense == True
        W['YardLine_std'] = 100 - W.yardlineNumber
        W.loc[W.yardlineSide.fillna('') == W.possessionTeam,  
                'YardLine_std'
                ] = W.loc[W.yardlineSide.fillna('') == W.possessionTeam,  
                'yardlineNumber']
        W['X_std'] = W.x
        W.loc[W.ToLeft, 'X_std'] = 120 - W.loc[W.ToLeft, 'x'] 
        W['Y_std'] = W.y
        W.loc[W.ToLeft, 'Y_std'] = 160/3 - W.loc[W.ToLeft, 'y'] 
        W['Dir_std'] = W.Dir_rad
        W.loc[W.ToLeft, 'Dir_std'] = np.mod(np.pi + W.loc[W.ToLeft, 'Dir_rad'], 2*np.pi)
        W['dx'] = round(W['s']*np.cos(W['Dir_std']),2)
        W['dy'] = round(W['s']*np.sin(W['Dir_std']),2)
        W['X_std'] = round(W['X_std'],2)
        W['Y_std'] = round(W['Y_std'],2)
        W['Orientation_rad'] = np.mod(-W.o + 90, 360) * math.pi/180.0
        W['Orientation_std'] = W.Orientation_rad
        W.loc[W.ToLeft, 'Orientation_std'] = np.mod(np.pi + W.loc[W.ToLeft, 'Orientation_rad'], 2*np.pi)
        return W



class GetXY():
    def __init__(self,Create) -> None:
        self.Create = Create
        super().__init__()
    
    def GetBDB(self):
        print("Load BDB..")
        start = time.process_time()
        Weeks = range(1,18)

        data = []
        for n in tqdm(Weeks):
            filename = 'BDB Data/week' + str(n) + '.csv'
            frame = pd.read_csv(filename)
            events = ['ball_snap', 'pass_arrived','pass_outcome_caught']
            frame = frame.loc[(frame['event'].isin(events))]
            frame['Code'] = frame['gameId'].astype(str) + "-" + frame['playId'].astype(str)
            plays = pd.read_csv('BDB Data/plays.csv')
            plays['Code'] = plays['gameId'].astype(str) + "-" + plays['playId'].astype(str)
            plays = plays.set_index('Code')
            plays = plays.loc[~plays.index.duplicated(keep='first')]
            plays = plays.drop(['gameId','playId'], axis=1)
            frame = frame.set_index('Code').join(plays).reset_index()
            data.append(frame)

        Finaldf1 = pd.concat(data).reset_index()

        games = pd.read_csv('BDB Data/games.csv')
        games = games.set_index('gameId')

        Finaldf1 = Finaldf1.set_index('gameId').join(games).reset_index()
        Finaldf1['PlayerTeam'] = np.where(Finaldf1['team'] == 'home', Finaldf1['homeTeamAbbr'], Finaldf1['visitorTeamAbbr'] )
        Finaldf1['OppTeam'] = np.where(Finaldf1['team'] == 'home', Finaldf1['visitorTeamAbbr'], Finaldf1['homeTeamAbbr'] )
        Finaldf1['OnOffense'] = np.where(Finaldf1['PlayerTeam'] == Finaldf1['possessionTeam'], True, False)
        TargetedWR = pd.read_csv('BDB Data/targetedReceiver.csv')
        TargetedWR = TargetedWR.set_index(['gameId','playId'])#.join(plays).reset_index()
        Finaldf1 = Finaldf1.set_index(['gameId','playId']).join(TargetedWR).reset_index()
        Finaldf1['Target'] = np.where(Finaldf1['nflId'] == Finaldf1['targetNflId'], 1, 0)
        Coverages = pd.read_csv('BDB Data/coverages_week1.csv')
        Coverages = Coverages.set_index(['gameId','playId'])
        Finaldf1 = Finaldf1.set_index(['gameId','playId']).join(Coverages).reset_index()

        StandardizedPBP = Standardize(Finaldf1)


        StandardizedPBP['LeftSideOfField'] = np.where(StandardizedPBP['Y_std'] >= 26.65,1,0)
        StandardizedPBP['DistFromCenter'] = 26.65 - StandardizedPBP['Y_std']
        StandardizedPBP['DistFromSideline'] = np.where(StandardizedPBP['LeftSideOfField'] == 1, 53.3 - StandardizedPBP['Y_std'],StandardizedPBP['Y_std'])
        StandardizedPBP['AbsdistFromCenter'] = np.abs(26.65 - StandardizedPBP['Y_std'])

        LOS = StandardizedPBP.query('displayName == "Football" & event == "ball_snap"')
        LOS = LOS.groupby(['gameId','playId'])['X_std','Y_std'].agg('median').reset_index()
        LOS.columns = ['gameId','playId','LOSX','LOSY']
        LOS['Code'] = LOS['gameId'].astype(str) + "-" + LOS['playId'].astype(str)
        LOS = LOS.set_index('Code')
        LOS = LOS.loc[~LOS.index.duplicated(keep='first')]
        StandardizedPBP["LOSX"] = StandardizedPBP.Code.map(LOS['LOSX'])
        StandardizedPBP["LOSY"] = StandardizedPBP.Code.map(LOS['LOSY'])
        StandardizedPBP['Distfrom_LOSX'] = StandardizedPBP['X_std'] - StandardizedPBP['LOSX']
        StandardizedPBP['Distfrom_LOSY'] = StandardizedPBP['LOSY'] - StandardizedPBP['Y_std']
        StandardizedPBP['AbsDistfrom_LOSX'] = np.abs(StandardizedPBP['X_std'] - StandardizedPBP['LOSX'])
        StandardizedPBP['AbsDistfrom_LOSY'] = np.abs(StandardizedPBP['Y_std'] - StandardizedPBP['LOSY'])

        StandardizedPBP = StandardizedPBP.loc[StandardizedPBP['IsOnOffense'] == 1]

        StandardizedPBP['WR_Location'] = StandardizedPBP.where(StandardizedPBP['event'] == "ball_snap").groupby(['gameId','playId','LeftSideOfField'])['DistFromSideline'].rank(ascending=True)
        StandardizedPBP['WR_Location'] = StandardizedPBP.groupby(['gameId','playId','nflId'])['WR_Location'].transform('max')



        StandardizedPBP['Distfrom_LOSX_first'] = StandardizedPBP.groupby(['gameId','playId','nflId'])['Distfrom_LOSX'].transform('first')
        StandardizedPBP['DistfromCenter_first'] = StandardizedPBP.groupby(['gameId','playId','nflId'])['AbsdistFromCenter'].transform('first')
        StandardizedPBP['Y_Std_first'] = StandardizedPBP.groupby(['gameId','playId','nflId'])['Y_std'].transform('first')
        StandardizedPBP['LeftSideOfField_first'] = np.where(StandardizedPBP['Y_Std_first'] >= 26.65,1,0)

        StandardizedPBP['Location'] = np.where(StandardizedPBP['WR_Location'] == 1, "Outside", "Slot")

        StandardizedPBP['Location'] = np.where((StandardizedPBP['Distfrom_LOSX_first'] < -2.5) & (StandardizedPBP['LOSY'] <= 31) & (StandardizedPBP['LOSY'] >= 22), "Backfield", StandardizedPBP['Location'])

        StandardizedPBP['WR_Location'] = np.where(StandardizedPBP['Location'] == "Backfield", 10, StandardizedPBP['WR_Location'])


        StandardizedPBP = StandardizedPBP.loc[(StandardizedPBP["LOSY"] < 32) & (StandardizedPBP["LOSY"] >= 21)]
        StandardizedPBP = StandardizedPBP.loc[(StandardizedPBP['route'].notnull()) & (StandardizedPBP['event'] == "pass_arrived")]


        StandardizedPBP1 = StandardizedPBP.loc[(StandardizedPBP['route'].notnull()) & (StandardizedPBP['event'] == "pass_arrived")]
        StandardizedPBP1['Team_Score'] = np.where(StandardizedPBP1['team'] == "home", StandardizedPBP1['preSnapHomeScore'], StandardizedPBP1['preSnapVisitorScore'] )
        StandardizedPBP1['Opp_Score'] = np.where(StandardizedPBP1['team'] == "home", StandardizedPBP1['preSnapVisitorScore'], StandardizedPBP1['preSnapHomeScore'] )

        StandardizedPBP1['Score_diff'] = StandardizedPBP1['Team_Score'] - StandardizedPBP1['Opp_Score'] 

        StandardizedPBP1['Year'] = 2018

        StandardizedPBP1['Hash'] = np.where(StandardizedPBP1['LOSY'] >= 29.65, 1, 2)
        StandardizedPBP1['Hash'] = np.where(StandardizedPBP1['LOSY'] <= 23.65, 3, StandardizedPBP1['Hash'])


        Cols = ['Year','week', 'gameId', 'playId','nflId', 'displayName','position','PlayerTeam','OppTeam', 'Target', 'route','quarter', 'down', 'yardsToGo','YardLine_std', 'Team_Score', 
                'Opp_Score','Score_diff', 'gameClock','passResult', 'offensePlayResult','epa', 'coverage', 'DistFromCenter', 'DistFromSideline','Y_Std_first', 'Distfrom_LOSX','Distfrom_LOSY', 
                'WR_Location','LeftSideOfField_first', 'Location','Hash']

        StandardizedPBP1 = StandardizedPBP1[Cols]

        StandardizedPBP1['gameClock'] = pd.to_numeric(StandardizedPBP1['gameClock'].str.split(':').str[0], errors='coerce')*60 + pd.to_numeric(StandardizedPBP1['gameClock'].str.split(':').str[1], errors='coerce')

        StandardizedPBP1['passResult'] = np.where(StandardizedPBP1['passResult'] == "C", 1, 0)

        Cols = ['DistFromCenter','DistFromSideline', 'Distfrom_LOSY']

        for c in Cols:
            StandardizedPBP1[c] = np.where(StandardizedPBP1['LeftSideOfField_first'] == 1,StandardizedPBP1[c]*-1, StandardizedPBP1[c])


        Cols = ['Y_Std_first']

        for c in Cols:
            StandardizedPBP1[c] = np.where(StandardizedPBP1['LeftSideOfField_first'] == 0,(26.65 - StandardizedPBP1[c]) + 26.65, StandardizedPBP1[c])

        StandardizedPBP1["route"] = np.where((StandardizedPBP1['route'] == "CROSS") & (StandardizedPBP1['Distfrom_LOSX'] >= 8), "DEEPCROSS", StandardizedPBP1["route"]) 
        StandardizedPBP1["route"] = np.where((StandardizedPBP1['route'] == "CROSS") & (StandardizedPBP1['Distfrom_LOSX'] < 8) & (StandardizedPBP1['Distfrom_LOSX'] > 0), "DRAG", StandardizedPBP1["route"]) 
        StandardizedPBP1["route"] = np.where((StandardizedPBP1['route'] == "CROSS") & (StandardizedPBP1['Distfrom_LOSX'] <= 0), "FLAT", StandardizedPBP1["route"]) 
        StandardizedPBP1["route"] = np.where((StandardizedPBP1['route'] == "IN") & (StandardizedPBP1['Distfrom_LOSX'] >= 8), "DIG", StandardizedPBP1["route"]) 
        StandardizedPBP1["route"] = np.where((StandardizedPBP1['route'] == "OUT") & (StandardizedPBP1['WR_Location'] == 1) & (StandardizedPBP1['Distfrom_LOSX'] >= 7), "COMEBACK", StandardizedPBP1["route"]) 
        StandardizedPBP1["Distfrom_LOSY"] = np.where((StandardizedPBP1['route'] == "WHEEL") & (StandardizedPBP1['Distfrom_LOSY'] < 0), StandardizedPBP1['Distfrom_LOSY']*-1, StandardizedPBP1["Distfrom_LOSY"]) 
        StandardizedPBP1["Distfrom_LOSY"] = np.where((StandardizedPBP1['route'] == "HITCH") & (StandardizedPBP1['Distfrom_LOSY'] < 0), StandardizedPBP1['Distfrom_LOSY']*-1, StandardizedPBP1["Distfrom_LOSY"]) 
        return StandardizedPBP1

    def GetSIS(self):
        print("Load SIS..")
        Gameinfo = pd.read_csv("GameInfo.csv")
        PlayerTotalPoints = pd.read_csv("PlayerTotalPoints.csv")
        PBP = pd.read_csv("PlayByPlay.csv")
        PBP = PBP.set_index(['GameID','EventID'])
        SkillPlayers = pd.read_csv("SkillPositionPlayers.csv")
        SkillPlayers = SkillPlayers.set_index(['GameID','EventID']).join(PBP).reset_index()

        HITCH = ['Curl', 'Chip - Curl', ]
        OUT = ['Out','Whip']
        COMEBACK = ['Comeback']
        FLAT = ['Check & Release', 'Swing - Left',  'Jet Sweep Pass', 'Swing - Right','Chip - Flat','Flat - Left', 'Flat - Right', 'Chip',]
        #CROSS = ['Beneath','Drag','Chip - Drag', 'Over Ball',  'Pick', ]
        DRAG = ['Beneath','Drag','Chip - Drag', 'Over Ball',  'Pick', ]
        DEEPCROSS = ['Deep Cross']
        GO = ['Fade - Back Shoulder','Go/Fly','Fade','Sluggo','Hitch & Go','Seam', 'Chip - Seam']
        #SEAM = ['Seam', 'Chip - Seam']
        SLANT = ['Slant',]
        SCREEN = ['Screen - RB','Screen - Bubble', 'Screen - TE',  'Screen - Quick','Screen - Tunnel',  'Screen - Shovel', 'Screen - Beneath', 'Screen - Drag','Quick']
        CORNER = [ 'Corner','Post Corner', ]
        IN = ['Jerk', ]
        DIG = ['Dig']
        ANGLE = [ 'Angle', ]
        POST = [ 'Post', 'Stick - Nod', 'Corner Post',  ]
        WHEEL = [ 'Wheel', 'Out & Up','Leak']

        SkillPlayers['Play_Action'] = np.where(SkillPlayers['Route'] == "Run Fake", 1, 0)
        SkillPlayers['Play_Action'] = SkillPlayers.groupby(['GameID','EventID'])['Play_Action'].transform("max")

        SkillPlayers['Screen'] = np.where(SkillPlayers['Route'].str.contains("Screen", na=False), 1, 0)
        SkillPlayers['Screen'] = SkillPlayers.groupby(['GameID','EventID'])['Screen'].transform("max")

        SkillPlayers['Order_OutsideToInside'] = SkillPlayers['Order_OutsideToInside'].fillna(10).astype(int)

        SkillPlayers = SkillPlayers.loc[(SkillPlayers['EventType'] == "pass") & (SkillPlayers['Spike'] == 0)& (SkillPlayers['FumbleByReceiver'] == 0) & (SkillPlayers['SackOnPlay'] == 0) & (SkillPlayers['ThrowAway'] == 0)]

        NA_Routes = ["Blocking","Run Fake"]


        SkillPlayers['Route'] = np.where(SkillPlayers['Route'].isin(NA_Routes), np.nan, SkillPlayers['Route'])

        Backfield_left = [ 'Swing - Left', 'Flat - Left']

        SkillPlayers['SideOfCenter'] = np.where((SkillPlayers['Route'].str.contains("Left")) & (SkillPlayers['SideOfCenter'].isnull()), "L", SkillPlayers['SideOfCenter'])
        SkillPlayers['SideOfCenter'] = np.where((SkillPlayers['Route'].str.contains("Right")) & (SkillPlayers['SideOfCenter'].isnull()), "R", SkillPlayers['SideOfCenter'])


        SkillPlayers['New_Route'] = np.where(SkillPlayers['Route'].isin(HITCH), "HITCH", np.nan)
        SkillPlayers['New_Route'] = np.where(SkillPlayers['Route'].isin(OUT), "OUT", SkillPlayers['New_Route'])
        SkillPlayers['New_Route'] = np.where(SkillPlayers['Route'].isin(FLAT), "FLAT", SkillPlayers['New_Route'])
        SkillPlayers['New_Route'] = np.where(SkillPlayers['Route'].isin(DRAG), "DRAG", SkillPlayers['New_Route'])
        SkillPlayers['New_Route'] = np.where(SkillPlayers['Route'].isin(GO), "GO", SkillPlayers['New_Route'])
        SkillPlayers['New_Route'] = np.where(SkillPlayers['Route'].isin(SLANT), "SLANT", SkillPlayers['New_Route'])
        SkillPlayers['New_Route'] = np.where(SkillPlayers['Route'].isin(SCREEN), "SCREEN", SkillPlayers['New_Route'])
        SkillPlayers['New_Route'] = np.where(SkillPlayers['Route'].isin(CORNER), "CORNER", SkillPlayers['New_Route'])
        SkillPlayers['New_Route'] = np.where(SkillPlayers['Route'].isin(IN), "IN", SkillPlayers['New_Route'])
        SkillPlayers['New_Route'] = np.where(SkillPlayers['Route'].isin(ANGLE), "ANGLE", SkillPlayers['New_Route'])
        SkillPlayers['New_Route'] = np.where(SkillPlayers['Route'].isin(POST), "POST", SkillPlayers['New_Route'])
        SkillPlayers['New_Route'] = np.where(SkillPlayers['Route'].isin(WHEEL), "WHEEL", SkillPlayers['New_Route'])
        SkillPlayers['New_Route'] = np.where(SkillPlayers['Route'].isin(DEEPCROSS), "DEEPCROSS", SkillPlayers['New_Route'])
        SkillPlayers['New_Route'] = np.where(SkillPlayers['Route'].isin(COMEBACK), "COMEBACK", SkillPlayers['New_Route'])
        SkillPlayers['New_Route'] = np.where(SkillPlayers['Route'].isin(DIG), "DIG", SkillPlayers['New_Route'])
        SkillPlayers['New_Route'] = SkillPlayers['New_Route'].replace("nan", np.nan)
        SkillPlayers['Order_OutsideToInside'] = pd.to_numeric(SkillPlayers['Order_OutsideToInside'], errors='coerce')

        SkillPlayers['Location'] = np.where(SkillPlayers['Order_OutsideToInside'] > 1, "Slot",np.nan)
        SkillPlayers['Location'] = np.where(SkillPlayers['OnFieldPosition'] == "B", "Backfield",SkillPlayers['Location'] )
        SkillPlayers['Location'] = np.where(SkillPlayers['Order_OutsideToInside'] == 1, "Outside",SkillPlayers['Location'] )

        SkillPlayers['LeftSideOfField_first'] = np.where(SkillPlayers['SideOfCenter'] == "L",1,np.nan)
        SkillPlayers['LeftSideOfField_first'] = np.where(SkillPlayers['SideOfCenter'] == "R",0,SkillPlayers['LeftSideOfField_first'])

        SkillPlayers['YardLine_std'] = np.where(SkillPlayers['SideOfField'] == "Oppo", 50 + (50-SkillPlayers['StartYard']), SkillPlayers['StartYard'])
        SkillPlayers['YardLine_std'] = np.where(SkillPlayers['YardLine_std'] == 50, 50, SkillPlayers['YardLine_std'])

        SkillPlayers['Score_diff'] = SkillPlayers['OffTeamScoreBefore'] - SkillPlayers['DefTeamScoreBefore']


 #       print(SkillPlayers.columns.to_list())


        Cols = [ 'Season', 'Week','GameID', 'EventID', 'PlayerId', 'Name', 'OffensiveTeam', 'DefensiveTeam', 'RosterPosition','Target','Route','New_Route', 'Quarter', 'Down','ToGo','YardLine_std', 
            'OffTeamScoreBefore', 'DefTeamScoreBefore','Score_diff','TimeLeft','Completion', 'OffensiveYardage','EPA','CoverageScheme','ThrowDepth', 'Order_OutsideToInside', 'Hash', 'Location','LeftSideOfField_first',
            'FastMotion','Touchdown', 'Shotgun', 'DropType', 'RPO','Play_Action', 'Screen']

        SkillPlayers = SkillPlayers[Cols]

  #      print(SkillPlayers.columns.to_list())

        SkillPlayers = SkillPlayers.loc[SkillPlayers['New_Route'].notnull()]

        SkillPlayers.columns = ['Year', 'week', 'gameId', 'playId', 'nflId', 'displayName', 'PlayerTeam', 'OppTeam','position','Target', 'Old_route','route', 'quarter', 
                            'down', 'yardsToGo', 'YardLine_std', 'Team_Score', 'Opp_Score', 'Score_diff', 'gameClock', 'passResult', 'offensePlayResult', 'epa',
                            'coverage', 'Distfrom_LOSX', 'WR_Location', 'Hash','Location', 'LeftSideOfField_first','FastMotion','Touchdown', 'Shotgun', 'DropType', 'RPO','Play_Action', 'Screen']
        return SkillPlayers

    def Combine(self, StandardizedPBP1, SkillPlayers):
        Combined = pd.concat([StandardizedPBP1, SkillPlayers])
        Combined['Players_on_side'] = Combined.groupby(['gameId','playId','LeftSideOfField_first'])['nflId'].transform('count')

        Combined['Num_WRs'] = np.where(Combined['position'] == "WR", 1, 0)
        Combined['Num_WRs'] = Combined.groupby(['gameId','playId'])['Num_WRs'].transform('sum')

        Combined['Num_TEs'] = np.where(Combined['position'] == "TE", 1, 0)
        Combined['Num_TEs'] = Combined.groupby(['gameId','playId'])['Num_TEs'].transform('sum')

        Combined['Num_RBs'] = np.where((Combined['position'] == "RB") | (Combined['position'] == "FB") | (Combined['position'] == "HB") , 1, 0)
        Combined['Num_RBs'] = Combined.groupby(['gameId','playId'])['Num_RBs'].transform('sum')

        Combined['Total_Pos'] = Combined['Num_WRs'].fillna(0) + Combined['Num_TEs'].fillna(0) + Combined['Num_RBs'].fillna(0)


        Combined['Num_WRs_On_side'] = np.where(Combined['position'] == "WR", 1, 0)
        Combined['Num_WRs_On_side'] = Combined.groupby(['gameId','playId', 'LeftSideOfField_first'])['Num_WRs_On_side'].transform('sum')

        Combined['Num_TEs_On_side'] = np.where(Combined['position'] == "TE", 1, 0)
        Combined['Num_TEs_On_side'] = Combined.groupby(['gameId','playId', 'LeftSideOfField_first'])['Num_TEs_On_side'].transform('sum')

        Combined['Num_RBs_On_side'] = np.where((Combined['position'] == "RB") | (Combined['position'] == "FB") | (Combined['position'] == "HB") , 1, 0)
        Combined['Num_RBs_On_side'] = Combined.groupby(['gameId','playId', 'LeftSideOfField_first'])['Num_RBs_On_side'].transform('sum')

        Combined['Total_Pos_On_side'] = Combined['Num_WRs_On_side'].fillna(0) + Combined['Num_TEs_On_side'].fillna(0) + Combined['Num_RBs_On_side'].fillna(0)

        Combined['Pos_On_side_Ratio'] = Combined['Total_Pos_On_side'] / Combined['Total_Pos']
        return Combined

    def AddX(self, Combined):
        print("Add X..")
        Pred_x = pickle.load(open("Models/X_Depth.pkl", 'rb'))
        scale_x = pickle.load(open("Models/X_scale.pkl", 'rb'))


        Cols = ['position','route','quarter','down','yardsToGo','YardLine_std','Score_diff','gameClock','Location','WR_Location','Players_on_side',
                'Num_WRs','Num_TEs','Num_RBs','Total_Pos','Num_WRs_On_side','Num_TEs_On_side','Num_RBs_On_side','Total_Pos_On_side','Pos_On_side_Ratio']


        X = Combined[Cols]

        X = X.fillna(-999)

        numeric_features = ['quarter','down','yardsToGo','YardLine_std','Score_diff','gameClock','WR_Location','WR_Location','Players_on_side',
                'Num_WRs','Num_TEs','Num_RBs','Total_Pos','Num_WRs_On_side','Num_TEs_On_side','Num_RBs_On_side','Total_Pos_On_side','Pos_On_side_Ratio']

        X[numeric_features] = scale_x.transform(X[numeric_features])
        Combined['X_Depth'] = Pred_x.predict(X)
        return Combined

    def AddY(self, Combined):
        print("Add Y..")
        Pred_y = pickle.load(open("Models/Y_Depth.pkl", 'rb'))
        scale_y = pickle.load(open("Models/Y_scale.pkl", 'rb'))


        Cols = ['position','route','quarter','down','yardsToGo','YardLine_std','Score_diff','gameClock','Location','WR_Location','Players_on_side',
                'Num_WRs','Num_TEs','Num_RBs','Total_Pos','Num_WRs_On_side','Num_TEs_On_side','Num_RBs_On_side','Total_Pos_On_side','Pos_On_side_Ratio','X_Depth']


        X = Combined[Cols]

        X = X.fillna(-999)

        numeric_features = ['quarter','down','yardsToGo','YardLine_std','Score_diff','gameClock','WR_Location','WR_Location','Players_on_side',
                'Num_WRs','Num_TEs','Num_RBs','Total_Pos','Num_WRs_On_side','Num_TEs_On_side','Num_RBs_On_side','Total_Pos_On_side','Pos_On_side_Ratio','X_Depth']

        X[numeric_features] = scale_y.transform(X[numeric_features])
        Combined['Y_Depth'] = Pred_y.predict(X)
        return Combined
    
    def FlipY(self, Combined):
        Cols = ['Y_Depth','Distfrom_LOSY']

        for c in Cols:
            Combined[c] = np.where(Combined['LeftSideOfField_first'] == 1,Combined[c]*-1, Combined[c])

        Coverages = Combined.loc[Combined['coverage'].notnull()]
        Coverages['Target_side'] = np.where(Coverages['Target'] == 1, Coverages['LeftSideOfField_first'],np.nan)
        Coverages['Target_side'] = Coverages.groupby(['gameId','playId'])['Target_side'].transform('max')
        Coverages['On_Target_side'] = np.where(Coverages['LeftSideOfField_first'] == Coverages['Target_side'], 1, 0)
        Coverages['Players_On_Target_side'] = Coverages.groupby(['gameId','playId'])['On_Target_side'].transform('sum')
        Coverages.to_csv("Coverages.csv")
        return Coverages


    def PlotPlay(self, Combined1, Gameid, Playid):

        import matplotlib.pyplot as plt
        import matplotlib.patches as patches
        import matplotlib._color_data as mcd
        import matplotlib.patches as mpatch
        import random

        import numpy as np
        import matplotlib.pyplot as plt
        import matplotlib.pyplot as plt
        plt.style.use(['seaborn-deep'])
        #mpl.style.use('presentation')

        overlap = [name for name in mcd.CSS4_COLORS]
        overlap2 = [name for name in mcd.XKCD_COLORS]

        overlap = list(set(overlap + overlap2))

        Example = Combined1.loc[(Combined1['playId'] == Playid) & (Combined1['gameId'] == Gameid)] 

        Example['Side'] = np.where(Example['LeftSideOfField_first'] == 1, "Left", np.nan)
        Example['Side'] = np.where(Example['LeftSideOfField_first'] == 0, "Right", Example['Side'])
        Example['Receiver_code'] = Example['Side'].astype(str) + "_" + Example['Location'].astype(str)  + "_" + Example['route'].astype(str)

        ydstogo = Example['yardsToGo'].iloc[0]

        Details = Example['PlayerTeam'].iloc[0] + " vs. " + Example['OppTeam'].iloc[0] + " - " + "Q" + Example['quarter'].iloc[0].astype(str) + " - " + Example['gameClock'].iloc[0].astype(str) + " Seconds Remaining - " \
                + Example['down'].iloc[0].astype(str) + "down & " + Example['yardsToGo'].iloc[0].astype(str) + "yds to go" + " - Score: " + \
                Example['Team_Score'].iloc[0].astype(str) + " to " + Example['Opp_Score'].iloc[0].astype(str) + " - EPA: " + np.round(Example['epa'].iloc[0],2).astype(str) + " - Coverage: " + Example['coverage'].iloc[0]

        routes = Example['Receiver_code'].to_list()

        print(str(Gameid) + " " + str(Playid) + ":", sorted(routes))


        fig, ax = plt.subplots()

        fig.set_size_inches(20,10)

        ax.plot()


        for r in routes:
            trim = Example.loc[Example['Receiver_code'] == r]
            color = overlap[random.randint(1,1090)]
            if trim["Target"].iloc[0] == 1:
                rect = patches.Rectangle((trim["Y_Depth"].iloc[0] - 6, trim["X_Depth"].iloc[0]-6),
                                (6*2),
                                (6*2),
                                linewidth=7,
                                edgecolor= 'red',
                                facecolor = color,
                                alpha=.5,
                                fill = True,
                                label=trim["Receiver_code"].iloc[0],)
        #     ax.legend()
                plt.text(x=trim["Y_Depth"]-3, y=trim["X_Depth"]+1, s= trim["Receiver_code"].iloc[0], size=15, weight='bold')
                plt.scatter(trim["Y_Depth"].iloc[0], trim["X_Depth"].iloc[0], s =30, c='black')
                overlap.remove(color)
                ax.add_patch(rect)
            else:
                rect = patches.Rectangle((trim["Y_Depth"].iloc[0] - 6, trim["X_Depth"].iloc[0] - 6),
                        (6*2),
                        (6*2),
                        linewidth=3,
                        edgecolor= 'black',
                        facecolor = color,
                        alpha=.5,
                        fill = True,
                        label=trim["Receiver_code"].iloc[0],)
            #   ax.legend()
                plt.text(x=trim["Y_Depth"].iloc[0]-4, y=trim["X_Depth"].iloc[0]+1, s= trim["Receiver_code"].iloc[0], size=15, weight='bold')
                plt.scatter(trim["Y_Depth"].iloc[0], trim["X_Depth"].iloc[0], s =30, c='black')
                overlap.remove(color)
                ax.add_patch(rect)

        #ax.hlines(title_fontsize= 'xx-large')
        plt.axhline(y=0, color='r', linestyle='-',linewidth=6,alpha=.5)
        plt.text(x=-28, y=1, s= "LOS", size=20, color='blue', weight='bold')

        plt.axhline(y=ydstogo, color= 'gold', linestyle='-',linewidth=6,alpha=.5)
        plt.text(x=-28, y=ydstogo + 1, s= "First Down", size=20, color='blue', weight='bold')

        rect = patches.Rectangle((-28, 28), 0, 50, linewidth=0.1,
                                    edgecolor='r', facecolor='darkgreen', zorder=0)

        ax.add_patch(rect)

        fig.suptitle(Details, fontsize=20)

        plt.ylim([-10, 40])
        plt.xlim([-28, 28])
        return plt.show()

    

    def GetRelativePlayers(self, weekMod):
            import heapq
            groupedWeek = weekMod.groupby(['gameId', 'playId'])
            playerXY = {}
            for name, group in tqdm(groupedWeek):
                playerXY[name] = []
                for row in group.iterrows():
                    data = [row[1]['nflId'], row[1]['PlayerTeam'], row[1]['X_Depth'], row[1]['Y_Depth'], row[1]['position'], row[1]['route']]
                    playerXY[name].append(data)

            # 5 Closest Teammates
            
            features = list(weekMod.columns)
            weekArray = np.array(weekMod)
            minMateDist = []
            for player in tqdm(weekArray):
                try:
                    matePositions = playerXY[(player[features.index('gameId')], player[features.index('playId')])]
                    distancesTm = []
                    mates = []
                    xs = []
                    ys = []
                    for matePos in matePositions:
                        if player[features.index('PlayerTeam')] == matePos[1] and player[features.index('nflId')] != matePos[0]:
                            dx = (player[features.index('X_Depth')] - matePos[2])**2
                            dy = (player[features.index('Y_Depth')] - matePos[3])**2
                            dist = np.sqrt(dx+dy)
                            distancesTm.append(dist)
                            mates.append(matePos[0])
                    try:
                        TmDist_1 = heapq.nsmallest(5, distancesTm)[0]
                        Close1stIdx = np.array(np.where(distancesTm == heapq.nsmallest(8, distancesTm)[0]))[0][0]
                        CT1 = mates[Close1stIdx]

                    except:
                        TmDist_1 = np.nan
                        CT1 = np.nan
                    try:
                        TmDist_2 = heapq.nsmallest(5, distancesTm)[1]
                        Close2ndIdx = np.array(np.where(distancesTm == heapq.nsmallest(8, distancesTm)[1]))[0][0]
                        CT2 = mates[Close2ndIdx]
                    except:
                        TmDist_2 = np.nan
                        CT2 = np.nan

                    try:
                        TmDist_3 = heapq.nsmallest(5, distancesTm)[2]
                        Close3rdIdx = np.array(np.where(distancesTm == heapq.nsmallest(8, distancesTm)[2]))[0][0]
                        CT3 = mates[Close3rdIdx]
                    except:
                        TmDist_3 = np.nan
                        CT3 = np.nan
                    try:
                        TmDist_4 = heapq.nsmallest(5, distancesTm)[3]
                        Close4thIdx = np.array(np.where(distancesTm == heapq.nsmallest(8, distancesTm)[3]))[0][0]
                        CT4 = mates[Close4thIdx]
                    except:
                        TmDist_4 = np.nan
                        CT4 = np.nan
                    try:
                        TmDist_5 = heapq.nsmallest(5, distancesTm)[4]
                        Close5thIdx = np.array(np.where(distancesTm == heapq.nsmallest(8, distancesTm)[4]))[0][0]
                        CT5 = mates[Close5thIdx]
                    except:
                        TmDist_5 = np.nan
                        CT5 = np.nan
                except:
                    continue
        #      summary = [player[features.index('gameId')], player[features.index('playId')], player[features.index('nflId')], TmDist_1, CT1]
                summary = [player[features.index('Year')],player[features.index('week')],player[features.index('gameId')], player[features.index('playId')], player[features.index('nflId')], player[features.index('displayName')], player[features.index('route')],player[features.index('position')], player[features.index('Target')], TmDist_1, CT1,TmDist_2,CT2,TmDist_3,CT3,TmDist_4,CT4,TmDist_5,CT5]
                minMateDist.append(summary)


            minMateDist = pd.DataFrame(minMateDist, columns=['Year','week','gameId', 'playId', 'nflId','displayName','route','tar_pos','Target','TmDist_1', 'CT1','TmDist_2', 'CT2','TmDist_3', 'CT3','TmDist_4', 'CT4','TmDist_5', 'CT5'])
            return minMateDist

    def CleanCoverages(self, Coverages):
        Coverages['Inv'] = False

        Coverages['route'] = np.where(Coverages['Old_route'] == "Leak", "LEAK", Coverages['route'])
        Coverages['Y_Depth'] = np.where(Coverages['route'] == "LEAK", Coverages['Y_Depth']*-1, Coverages['Y_Depth'])

        Coverages['route'] = np.where(Coverages['Old_route'] == "Beneath", "BENEATH", Coverages['route'])
        Coverages['route'] = np.where(Coverages['Old_route'] == "Whip", "WHIP", Coverages['route'])
        Coverages['route'] = np.where(Coverages['Old_route'] == "Jerk", "JERK", Coverages['route'])
        Coverages['route'] = np.where(Coverages['Old_route'] == "Over Ball", "OVERBALL", Coverages['route'])
        Coverages['route'] = np.where(Coverages['Old_route'] == "Pick", "PICK", Coverages['route'])
        Coverages['route'] = np.where(Coverages['Old_route'] == "Dig", "DIG", Coverages['route'])
        Coverages['route'] = np.where(Coverages['Old_route'] == "Comeback", "COMEBACK", Coverages['route'])
        Coverages['route'] = np.where(Coverages['Old_route'] == "Deep Cross", "DEEPCROSS", Coverages['route'])
        Coverages['route'] = np.where(Coverages['Old_route'] == "Angle", "ANGLE", Coverages['route'])
        Coverages['route'] = np.where(Coverages['Old_route'] == "Sluggo", "SLUGGO", Coverages['route'])
        Coverages['route'] = np.where(Coverages['Old_route'] == "Out & Up", "OUTUP", Coverages['route'])
        Coverages['route'] = np.where(Coverages['Old_route'] == "Stick - Nod", "STICKNOD", Coverages['route'])
        Coverages['route'] = np.where(Coverages['Old_route'] == "Hitch & Go", "HITCHGO", Coverages['route'])
        Coverages['route'] = np.where(Coverages['Old_route'] == "Corner Post", "CORNERPOST", Coverages['route'])
        Coverages['route'] = np.where(Coverages['Old_route'] == "Post Corner", "POSTCORNER", Coverages['route'])
        Coverages['route'] = np.where(Coverages['Old_route'] == "Screen - Bubble", "BUBBLE", Coverages['route'])

        Cov = [ 
            'Screen',
            'Combination','Other']

        Cover0 = ['Cover 0 Man', 'Cover 0', ]
        Cover1 = ['Cover 1 Man', 'Cover 1',]
        Cover2 = ['Cover 2 Zone', 'Cover 2',]
        Cover2Man = ['Cover 2 Man', 'Man Cover 2']
        Cover3 = ['Man Cover 3', 'Cover 3', 'Tampa 2' ]
        Cover4 = ['Cover 4 Zone',  'Cover 4',  'Cover 4','Cover 4 Zone']
        Cover6 = ['Cover 6', 'Cover 6 Zone']
        Prevent = ['Prevent Zone', 'Prevent']

        Coverages['Success'] = np.where(Coverages['offensePlayResult'] >= Coverages['yardsToGo'],1,0)

        Coverages['NewCoverage'] = np.where(Coverages['coverage'].isin(Cover0), "Cov0", "Other")
        Coverages['NewCoverage'] = np.where(Coverages['coverage'].isin(Cover1), "Cov1", Coverages['NewCoverage'])
        Coverages['NewCoverage'] = np.where(Coverages['coverage'].isin(Cover2), "Cov2", Coverages['NewCoverage'])
        Coverages['NewCoverage'] = np.where(Coverages['coverage'].isin(Cover2Man), "Cov2Man", Coverages['NewCoverage'])
        Coverages['NewCoverage'] = np.where(Coverages['coverage'].isin(Cover3), "Cov3", Coverages['NewCoverage'])
        Coverages['NewCoverage'] = np.where(Coverages['coverage'].isin(Cover4), "Cov4", Coverages['NewCoverage'])
        Coverages['NewCoverage'] = np.where(Coverages['coverage'].isin(Cover6), "Cov6", Coverages['NewCoverage'])
        Coverages['NewCoverage'] = np.where(Coverages['coverage'].isin(Prevent), "Prev", Coverages['NewCoverage'])

        Coverages['position'] = np.where(Coverages['position'] == "FB", "RB", Coverages['position'])

        Coverages["NewCoverage_cat"] = Coverages["NewCoverage"].astype('category').cat.codes

        Coverages['Target_side'] = np.where(Coverages['Target'] == 1, Coverages['LeftSideOfField_first'],np.nan)
        Coverages['Target_side'] = Coverages.groupby(['gameId','playId','Inv'])['Target_side'].transform('max')
        Coverages['On_Target_side'] = np.where(Coverages['LeftSideOfField_first'] == Coverages['Target_side'], 1, 0)
        Coverages['Players_On_Target_side'] = Coverages.groupby(['gameId','playId','Inv'])['On_Target_side'].transform('sum')
        Coverages['PointA'] = list(zip((Coverages['Y_Depth'] - 6), (Coverages['X_Depth'] - 6)))
        Coverages['PointB'] = list(zip((Coverages['Y_Depth'] - 6), (Coverages['X_Depth'] + 6)))
        Coverages['PointC'] = list(zip((Coverages['Y_Depth'] + 6), (Coverages['X_Depth'] + 6)))
        Coverages['PointD'] = list(zip((Coverages['Y_Depth'] + 6), (Coverages['X_Depth'] - 6)))

        Coverages['Num_WRs_On_targetside'] = np.where(Coverages['Target'] == 1, Coverages['Num_WRs_On_side'], np.nan)
        Coverages['Num_WRs_On_targetside'] = Coverages.groupby(['gameId','playId','Inv'])['Num_WRs_On_targetside'].transform('max')
        Coverages['Num_WRs_On_targetside'] = Coverages['Num_WRs_On_targetside'].fillna(0)

        Coverages['Num_TEs_On_targetside'] = np.where(Coverages['Target'] == 1, Coverages['Num_TEs_On_side'], np.nan)
        Coverages['Num_TEs_On_targetside'] = Coverages.groupby(['gameId','playId','Inv'])['Num_TEs_On_targetside'].transform('max')
        Coverages['Num_TEs_On_targetside'] = Coverages['Num_TEs_On_targetside'].fillna(0)

        Coverages['Num_RBs_On_targetside'] = np.where(Coverages['Target'] == 1, Coverages['Num_RBs_On_side'], np.nan)
        Coverages['Num_RBs_On_targetside'] = Coverages.groupby(['gameId','playId','Inv'])['Num_RBs_On_targetside'].transform('max')
        Coverages['Num_RBs_On_targetside'] = Coverages['Num_RBs_On_targetside'].fillna(0)

        Coverages['Pos_Ratio_targetside'] = np.where(Coverages['Target'] == 1, Coverages['Pos_On_side_Ratio'], np.nan)
        Coverages['Pos_Ratio_targetside'] = Coverages.groupby(['gameId','playId','Inv'])['Pos_Ratio_targetside'].transform('max')
        Coverages['Pos_Ratio_targetside'] = Coverages['Pos_Ratio_targetside'].fillna(0)

        Coverages['Play_contains_motion'] = np.where(Coverages['FastMotion'] == 1, 1, 0)
        Coverages['Play_contains_motion'] = Coverages.groupby(['gameId','playId','Inv'])['Play_contains_motion'].transform('max')

        Coverages['Motion_targetside'] = np.where((Coverages['FastMotion'] == 1) & (Coverages['On_Target_side'] == 1), 1, 0)
        Coverages['Motion_targetside'] = Coverages.groupby(['gameId','playId','Inv'])['Motion_targetside'].transform('max')

        Coverages['Motion_target'] = np.where((Coverages['FastMotion'] == 1) & (Coverages['Target'] == 1), 1, 0)
        Coverages['Motion_target'] = Coverages.groupby(['gameId','playId','Inv'])['Motion_target'].transform('max')

        Coverages['Loc_route'] = Coverages['WR_Location'].astype(int).astype(str) + Coverages['route'].astype(str) 


        Coverages['Target_side_routes'] = Coverages.groupby(['gameId', 'playId','Inv','On_Target_side'])['Loc_route'].transform(lambda x: ','.join(sorted(x)))
        Coverages['Target_side_routes'] = np.where(Coverages['On_Target_side'] == 1, Coverages['Target_side_routes'], np.nan)
        Coverages['Target_side_routes'] = Coverages['Target_side_routes'].fillna(Coverages.groupby(['gameId', 'playId','Inv'])['Target_side_routes'].transform('first'))
        Coverages['Target_side_routes'] = Coverages['Target_side_routes'].fillna(Coverages.groupby(['gameId', 'playId','Inv'])['Target_side_routes'].transform('last'))

        Coverages['Back_side_routes'] = Coverages.groupby(['gameId', 'playId','Inv','On_Target_side'])['Loc_route'].transform(lambda x: ','.join(sorted(x)))
        Coverages['Back_side_routes'] = np.where(Coverages['On_Target_side'] == 0, Coverages['Back_side_routes'], np.nan)
        Coverages['Back_side_routes'] = Coverages['Back_side_routes'].fillna(Coverages.groupby(['gameId', 'playId','Inv'])['Back_side_routes'].transform('first'))
        Coverages['Back_side_routes'] = Coverages['Back_side_routes'].fillna(Coverages.groupby(['gameId', 'playId','Inv'])['Back_side_routes'].transform('last'))

        Coverages['Target_Route'] = np.where(Coverages['Target'] == 1, Coverages['Loc_route'], np.nan)
        Coverages['Target_Route'] = Coverages['Target_Route'].fillna(Coverages.groupby(['gameId', 'playId','Inv'])['Target_Route'].transform('first'))
        Coverages['Target_Route'] = Coverages['Target_Route'].fillna(Coverages.groupby(['gameId', 'playId','Inv'])['Target_Route'].transform('last'))

        Coverages['Target_Pos'] = np.where(Coverages['Target'] == 1, Coverages['position'], np.nan)
        Coverages['Target_Pos'] = Coverages['Target_Pos'].fillna(Coverages.groupby(['gameId', 'playId','Inv'])['Target_Pos'].transform('first'))
        Coverages['Target_Pos'] = Coverages['Target_Pos'].fillna(Coverages.groupby(['gameId', 'playId','Inv'])['Target_Pos'].transform('last'))

        Coverages['Target_Loc'] = np.where(Coverages['Target'] == 1, Coverages['Loc_route'], np.nan)
        Coverages['Target_Loc'] = Coverages['Target_Loc'].fillna(Coverages.groupby(['gameId', 'playId','Inv'])['Target_Loc'].transform('first'))
        Coverages['Target_Loc'] = Coverages['Target_Loc'].fillna(Coverages.groupby(['gameId', 'playId','Inv'])['Target_Loc'].transform('last'))

        Coverages['Target_on_StrongSide'] = np.where(Coverages['Players_On_Target_side'] > (Coverages['Total_Pos'] - Coverages['Players_On_Target_side']), 1, 0)
        Coverages['Target_on_StrongSide'] = Coverages.groupby(['gameId','playId','Inv'])['Target_on_StrongSide'].transform('max')

        Coverages['Max_Slot_loc'] = Coverages.groupby(['gameId', 'playId','Inv','On_Target_side'])['WR_Location'].transform('max')
        Coverages['Max_Slot_loc'] = np.where(Coverages['Max_Slot_loc'] == 10, 1, Coverages['Max_Slot_loc'])
        Coverages['Max_Slot_loc'] = Coverages['Max_Slot_loc'].fillna(1)
        Coverages['Max_Slot_loc'] = Coverages['Max_Slot_loc'].astype(int)


        Coverages['Symmetrical'] = np.where(Coverages['Target_side_routes'] == Coverages['Back_side_routes'], 1, 0)


        Coverages['Hash'] = Coverages['Hash'].astype(int)

        hash_dict = {1:"L",
                     3:"R",
                     2:"M"}

        Coverages['Hash'] = Coverages['Hash'].replace(hash_dict)


        Coverages['Target_on_farside'] = np.where((Coverages['LeftSideOfField_first'] == 1) & (Coverages['Hash'] == "R") , 1, 0 )
        Coverages['Target_on_farside'] = np.where((Coverages['LeftSideOfField_first'] == 0) & (Coverages['Hash'] == "L") , 1, Coverages['Target_on_farside'] )
        Coverages['Target_on_farside'] = np.where(Coverages['Target'] == 1, Coverages['Target_on_farside'], np.nan)
        Coverages['Target_on_farside'] = Coverages.groupby(['gameId','playId','Inv'])['Target_on_farside'].transform('max')

        Coverages['Target_on_weakside'] = np.where((Coverages['LeftSideOfField_first'] == 1) & (Coverages['Hash'] == "L") , 1, 0 )
        Coverages['Target_on_weakside'] = np.where((Coverages['LeftSideOfField_first'] == 0) &  (Coverages['Hash'] == "R") , 1, Coverages['Target_on_weakside'] )
        Coverages['Target_on_weakside'] = np.where(Coverages['Target'] == 1, Coverages['Target_on_weakside'], np.nan)
        Coverages['Target_on_weakside'] = Coverages.groupby(['gameId','playId','Inv'])['Target_on_weakside'].transform('max')



        Coverages['Rollout'] = np.where(Coverages['DropType'].str.contains("Rollout",na=False), 1, 0 )
        Coverages['Rollout_farside'] = np.where((Coverages['DropType'].str.contains("Rollout Right")) & (Coverages['Hash'] == "L") , 1, 0 )
        Coverages['Rollout_farside'] = np.where((Coverages['DropType'].str.contains("Rollout Left")) & (Coverages['Hash'] == "R") , 1, Coverages['Rollout_farside'] )
        Coverages['Rollout_farside'] = Coverages.groupby(['gameId','playId','Inv'])['Rollout_farside'].transform('max')

        Coverages['Rollout_weakside'] = np.where((Coverages['DropType'].str.contains("Rollout Right")) & (Coverages['Hash'] == "R") , 1, 0 )
        Coverages['Rollout_weakside'] = np.where((Coverages['DropType'].str.contains("Rollout Left")) & (Coverages['Hash'] == "L") , 1, Coverages['Rollout_weakside'])
        Coverages['Rollout_weakside'] = Coverages.groupby(['gameId','playId','Inv'])['Rollout_weakside'].transform('max')

        Coverages['X_Depth'] = np.where(Coverages['route'] == "BENEATH", -1, Coverages['X_Depth'])
        Coverages['Y_Depth'] = np.where((Coverages['route'] == "PICK") & (Coverages['LeftSideOfField_first'] == 1), -6, Coverages['Y_Depth'])
        Coverages['Y_Depth'] = np.where((Coverages['route'] == "PICK") & (Coverages['LeftSideOfField_first'] == 0), 6, Coverages['Y_Depth'])
      #  Coverages['Y_Depth'] = np.where(Coverages['route'] == "OVERBALL", -4, Coverages['Y_Depth_test'])
        Coverages['Y_Depth'] = np.where((Coverages['route'] == "OVERBALL") & (Coverages['LeftSideOfField_first'] == 1), -1, Coverages['Y_Depth'])
        Coverages['Y_Depth'] = np.where((Coverages['route'] == "OVERBALL") & (Coverages['LeftSideOfField_first'] == 0), 1, Coverages['Y_Depth'])

        Coverages["Target_Player_X"] = np.where(Coverages["Target"] == 1, Coverages["X_Depth"], np.nan)
        Coverages['Target_Player_X'] = Coverages.groupby(['gameId','playId','Inv'])['Target_Player_X'].transform('max')

        Coverages["Target_Player_Y"] = np.where(Coverages["Target"] == 1, Coverages["Y_Depth"], np.nan)
        Coverages['Target_Player_Y'] = Coverages.groupby(['gameId','playId','Inv'])['Target_Player_Y'].transform('max')

        Coverages["Dist_from_Tar"] = np.sqrt((Coverages["X_Depth"] - Coverages["Target_Player_X"])**2 + (Coverages["Y_Depth"] - Coverages["Target_Player_Y"])**2)

        # Yards = [5,10,15,20]

        # for y in Yards:
        #     Coverages["Num_players_U"+str(y)] = np.where((Coverages["Dist_from_Tar"] <= y) & (Coverages["Target"] == 0), 1, 0)
        #     Coverages["Num_players_U"+str(y)] = Coverages.groupby(['gameId','playId','Inv'])["Num_players_U"+str(y)].transform('sum')
        #     Coverages["Num_TS_players_U"+str(y)] = np.where((Coverages["Dist_from_Tar"] <= y) & (Coverages["Target"] == 0) & (Coverages["On_Target_side"] == 1), 1, 0)
        #     Coverages["Num_TS_players_U"+str(y)] = Coverages.groupby(['gameId','playId','Inv'])["Num_TS_players_U"+str(y)].transform('sum') 
        #     Coverages["Num_BS_players_U"+str(y)] = np.where((Coverages["Dist_from_Tar"] <= y) & (Coverages["Target"] == 0) & (Coverages["On_Target_side"] == 0), 1, 0)
        #     Coverages["Num_BS_players_U"+str(y)] = Coverages.groupby(['gameId','playId','Inv'])["Num_BS_players_U"+str(y)].transform('sum')  


        Coverages['In_play'] = np.where((Coverages["Dist_from_Tar"] <= 20) | (Coverages["On_Target_side"] == 1), 1, 0)


        Cols = ['Y_Depth']

        for c in Cols:
            Coverages[c + "_test"] = np.where(Coverages['LeftSideOfField_first'] == 1,Coverages[c]*-1, Coverages[c])


        # Cols = ['Y_std_pred']

        # for c in Cols:
        #     Coverages[c+ "_test"] = np.where(Coverages['LeftSideOfField_first'] == 0,(26.65 - Coverages[c]) + 26.65, Coverages[c])


        Coverages = Coverages.loc[Coverages['Year'] == 2020]

        Coverages['X_Depth_level'] = np.where(Coverages['X_Depth'] < 8, "S", "D")
        Coverages['X_Depth_level'] = np.where((Coverages['X_Depth'] >= 8)  & (Coverages['X_Depth'] < 18) , "I", Coverages['X_Depth_level'])
        Coverages['X_Depth_level'] = np.where((Coverages['X_Depth'] >= 18), "D", Coverages['X_Depth_level'])

        Coverages['Y_Numbers'] = np.where((Coverages['Y_Depth_test'] >= 6), "Out", "In")

        Coverages['XY_level'] = Coverages['Y_Numbers'] + "_" + Coverages['X_Depth_level']

        Coverages['Loc_Xdepth'] = Coverages['WR_Location'].astype(int).astype(str) + Coverages['X_Depth_level'].astype(str) 
        Coverages['Loc_YNum'] = Coverages['WR_Location'].astype(int).astype(str) + Coverages['Y_Numbers'].astype(str) 
        Coverages['Loc_XY'] = Coverages['WR_Location'].astype(int).astype(str) + Coverages['XY_level'].astype(str) 


        #Clean DropType
        Coverages['DropType2'] = Coverages['DropType'].copy()

        DropType = ['3 Step', '0/1 Step', '5 Step', '7 Step', 'Basic Screen', 'RPO', 'Designed Rollout Left', 'Designed Rollout Right', 'RPO Move', 'Flea Flicker', 'Other', 'RB/WR Pass', 'WR Reverse Pass', 'Double Reverse Pass']

        #Remove trick/rare plays
        other = ['Flea Flicker', 'Other', 'RB/WR Pass', 'WR Reverse Pass', 'Double Reverse Pass']
        Coverages['DropType2'] = np.where(Coverages['DropType2'].isin(other), "Other",Coverages['DropType2'])

        #Combine Rollout
        RO = ['Designed Rollout Left', 'Designed Rollout Right']
        Coverages['DropType2'] = np.where(Coverages['DropType2'].isin(RO), "Rollout",Coverages['DropType2'])

        #Combine RPO
        RPO = ['RPO', 'RPO Move']
        Coverages['DropType2'] = np.where(Coverages['DropType2'].isin(RPO), "RPO",Coverages['DropType2'])

        Coverages = Coverages.loc[Coverages['DropType2'] != "Other"]


        Coverages['gameClock_half'] = np.where(Coverages['quarter'] <= 2, Coverages['gameClock'] - 450, Coverages['gameClock'])

        Coverages['RZ'] = np.where(Coverages['YardLine_std'] >= 80, 1, 0)
        Coverages['I10'] = np.where(Coverages['YardLine_std'] >= 90, 1, 0)
        Coverages['I5'] = np.where(Coverages['YardLine_std'] >= 95, 1, 0)
        Coverages['Dist_Bin'] = np.where(Coverages['yardsToGo'] >= 8, 3, 0)
        Coverages['Dist_Bin'] = np.where((Coverages['yardsToGo'] < 8) & (Coverages['yardsToGo'] >= 3), 2, Coverages['Dist_Bin'])
        Coverages['Dist_Bin'] = np.where((Coverages['yardsToGo'] < 3), 1, Coverages['Dist_Bin'])

        #Cols = ['route', 'X_Depth_level', 'Y_Numbers', 'XY_level','Loc_Xdepth','Loc_YNum','Loc_XY']

        Cols = ['route']

        for c in tqdm(Cols):
            Coverages['Target_side_' + c] = Coverages.groupby(['gameId', 'playId','Inv','On_Target_side'])[c].transform(lambda x: ','.join(sorted(x)))
            Coverages['Target_side_' + c] = np.where(Coverages['On_Target_side'] == 1,  Coverages['Target_side_' + c], np.nan)
            Coverages['Target_side_' + c] = Coverages['Target_side_' + c].fillna(Coverages.groupby(['gameId', 'playId','Inv'])['Target_side_' + c].transform('first'))
            Coverages['Target_side_' + c] = Coverages['Target_side_' + c].fillna(Coverages.groupby(['gameId', 'playId','Inv'])['Target_side_' + c].transform('last'))

            Coverages['Back_side_' + c] = Coverages.groupby(['gameId', 'playId','Inv','On_Target_side'])[c].transform(lambda x: ','.join(sorted(x)))
            Coverages['Back_side_' + c] = np.where(Coverages['On_Target_side'] == 0, Coverages['Back_side_' + c], np.nan)
            Coverages['Back_side_' + c] = Coverages['Back_side_' + c].fillna(Coverages.groupby(['gameId', 'playId','Inv'])['Back_side_' + c].transform('first'))
            Coverages['Back_side_' + c] = Coverages['Back_side_' + c].fillna(Coverages.groupby(['gameId', 'playId','Inv'])['Back_side_' + c].transform('last'))

            Coverages['Target_' + c] = np.where(Coverages['Target'] == 1, Coverages[c], np.nan)
            Coverages['Target_' + c] = Coverages['Target_' + c].fillna(Coverages.groupby(['gameId', 'playId','Inv'])['Target_' + c].transform('first'))
            Coverages['Target_' + c] = Coverages['Target_' + c].fillna(Coverages.groupby(['gameId', 'playId','Inv'])['Target_' + c].transform('last'))

        Coverages["Side"] = np.where(Coverages["On_Target_side"] == 1, "A","B")
        Coverages["Side_route"] = Coverages["Side"] + "_" + Coverages["route"]
        Coverages["Side_routes"] = Coverages.groupby(['gameId', 'playId','Inv'])['Side_route'].transform(lambda x: ','.join(sorted(x)))


        Coverages["Target_Side_Route"] = np.where(Coverages["Target"] == 1, Coverages["Side_route"], np.nan)
        Coverages["Target_Side_Route"] = Coverages["Target_Side_Route"].fillna(Coverages.groupby(['gameId', 'playId','Inv'])["Target_Side_Route"].transform('first'))
        Coverages["Target_Side_Route"] = Coverages["Target_Side_Route"].fillna(Coverages.groupby(['gameId', 'playId','Inv'])["Target_Side_Route"].transform('last'))


        Coverages["Side_routes_Inplay"] = Coverages.groupby(['gameId', 'playId','Inv','In_play'])['Side_route'].transform(lambda x: ','.join(sorted(x)))
        Coverages["Side_routes_Inplay"] = np.where(Coverages["In_play"] == 1, Coverages["Side_routes_Inplay"], np.nan)
        Coverages["Side_routes_Inplay"] = Coverages["Side_routes_Inplay"].fillna(Coverages.groupby(['gameId', 'playId','Inv'])["Side_routes_Inplay"].transform('first'))
        Coverages["Side_routes_Inplay"] = Coverages["Side_routes_Inplay"].fillna(Coverages.groupby(['gameId', 'playId','Inv'])["Side_routes_Inplay"].transform('last'))

        Coverages["routes_Inplay"] = Coverages.groupby(['gameId', 'playId','Inv','In_play'])['route'].transform(lambda x: ','.join(sorted(x)))
        Coverages["routes_Inplay"] = np.where(Coverages["In_play"] == 1, Coverages["routes_Inplay"], np.nan)
        Coverages["routes_Inplay"] = Coverages["routes_Inplay"].fillna(Coverages.groupby(['gameId', 'playId','Inv'])["routes_Inplay"].transform('first'))
        Coverages["routes_Inplay"] = Coverages["routes_Inplay"].fillna(Coverages.groupby(['gameId', 'playId','Inv'])["routes_Inplay"].transform('last'))


        Coverages["Target_Player"] = np.where(Coverages["Target"] == 1, Coverages["displayName"], np.nan)
        Coverages["Target_Player"] = Coverages["Target_Player"].fillna(Coverages.groupby(['gameId', 'playId','Inv'])["Target_Player"].transform('first'))
        Coverages["Target_Player"] = Coverages["Target_Player"].fillna(Coverages.groupby(['gameId', 'playId','Inv'])["Target_Player"].transform('last'))

        Coverages["Target_ID"] = np.where(Coverages["Target"] == 1, Coverages["displayName"], np.nan)
        Coverages["Target_ID"] = Coverages["Target_Player"].fillna(Coverages.groupby(['gameId', 'playId','Inv'])["Target_Player"].transform('first'))
        Coverages["Target_ID"] = Coverages["Target_Player"].fillna(Coverages.groupby(['gameId', 'playId','Inv'])["Target_Player"].transform('last'))
        Coverages.to_csv("Cleaned_Coverages.csv")
        return Coverages

    def Runit(self):
        if self.Create == True:
            BDB = self.GetBDB()
            SIS = self.GetSIS()
            Combined = self.Combine(BDB, SIS)
            XAdded = self.AddX(Combined)
            YAdded = self.AddY(XAdded)
            Coverages = self.FlipY(YAdded)
        else:
            Coverages = pd.read_csv("Coverages.csv", index_col=0)
        Coverages = self.CleanCoverages(Coverages)
        return Coverages

    def FindDoubles(self,Coverages,Relative):
        One = Relative[['Year','week','gameId', 'playId', 'nflId', 'displayName', 'tar_pos','route', 'Target', 'TmDist_1', 'CT1']]
        One.columns = ['Year','week','gameId', 'playId', 'tar_nflId', 'targetName', 'tar_pos', 'tar_route', 'Target', 'TmDist1', 'nflId1']
        One['Closest_rank'] = 1

        Two = Relative[['Year','week','gameId', 'playId', 'nflId', 'displayName', 'tar_pos', 'route', 'Target', 'TmDist_2', 'CT2']]
        Two.columns = ['Year','week','gameId', 'playId', 'tar_nflId', 'targetName',  'tar_pos','tar_route', 'Target', 'TmDist1', 'nflId1']
        Two['Closest_rank'] = 2

        Three = Relative[['Year','week','gameId', 'playId', 'nflId', 'displayName', 'tar_pos', 'route', 'Target', 'TmDist_3', 'CT3']]
        Three.columns = ['Year','week','gameId', 'playId', 'tar_nflId', 'targetName', 'tar_pos', 'tar_route', 'Target', 'TmDist1', 'nflId1']
        Three['Closest_rank'] = 3
        
        All = pd.concat([One, Two, Three])
        All = All.loc[All['Target'] == 1].dropna(subset=['TmDist1','nflId1']).sort_values(by=['Year','week','gameId','playId'])
        Cols = ['gameId', 'playId','nflId','Target_Side_Route','PlayerTeam','OppTeam', 'Score_diff','quarter','down', 'yardsToGo','YardLine_std', 
                    'gameClock','DropType2','RZ', 'Dist_Bin','Shotgun','RPO', 'Play_Action','Play_contains_motion', 'Motion_targetside', 'Motion_target','Target_on_farside', 
                    'Target_on_weakside','Rollout', 'Rollout_farside', 'Rollout_weakside','Num_WRs', 'Num_TEs', 'Num_RBs', 'Total_Pos','Num_WRs_On_targetside', 'Num_TEs_On_targetside', 
                    'Num_RBs_On_targetside', 'Players_On_Target_side', 'WR_Location','Location','X_Depth_level','Y_Numbers','epa','Success','NewCoverage']

        Details = Coverages[Cols]
        Details.columns = ['gameId', 'playId','tar_nflId','Target_Side_Route','PlayerTeam','OppTeam', 'Score_diff','quarter','down', 'yardsToGo','YardLine_std', 
                        'gameClock','DropType2','RZ', 'Dist_Bin','Shotgun','RPO', 'Play_Action','Play_contains_motion', 'Motion_targetside', 'Motion_target','Target_on_farside', 
                        'Target_on_weakside','Rollout', 'Rollout_farside', 'Rollout_weakside','Num_WRs', 'Num_TEs', 'Num_RBs', 'Total_Pos','Num_WRs_On_targetside', 'Num_TEs_On_targetside', 
                        'Num_RBs_On_targetside', 'Players_On_Target_side', 'tar_WR_Location','tar_Location','tar_X_Depth_level','tar_Y_Numbers','epa','Success','NewCoverage']
        Details = Details.set_index(['gameId','playId','tar_nflId'])
        All = All.set_index(['gameId','playId','tar_nflId']).join(Details).reset_index()

        for i in tqdm(range(1,2)):
            Cols = ['gameId', 'playId','nflId','position','route','On_Target_side', 'In_play','Side_route', 'WR_Location','Location','X_Depth_level','Y_Numbers']
            Match = Coverages[Cols]
            Match.columns = ['gameId', 'playId','nflId'+str(i),'position'+str(i),'route'+str(i),'On_Target_side'+str(i),'In_play'+str(i),'Side_route'+str(i),'WR_Location'+str(i),'Location'+str(i),'X_Depth_level'+str(i),'Y_Numbers'+str(i)]
            Match = Match.set_index(['gameId','playId','nflId'+str(i)])
            All = All.set_index(['gameId','playId','nflId'+str(i)]).join(Match).reset_index()

        All = All.loc[(All['In_play1'] == 1) ]

        
        All['Positions'] = All['tar_pos'] + "," + All['position1']
        All['Positions'] = All['Positions'].map(lambda x: ','.join(sorted(x.split(','))))

        All['Routes'] = All['tar_route'] + "," + All['route1']
        All['Routes'] = All['Routes'].map(lambda x: ','.join(sorted(x.split(','))))

        All['Routes'] = All['tar_route'] + "," + All['route1']
        All['Routes'] = All['Routes'].map(lambda x: ','.join(sorted(x.split(','))))

        All['Side_Routes'] = All['Target_Side_Route'] + "," + All['Side_route1'] 
        All['Side_Routes'] = All['Side_Routes'].map(lambda x: ','.join(sorted(x.split(','))))

        All['WR_Locations'] = All['tar_WR_Location'].astype(str) + "," + All['WR_Location1'].astype(str)
        All['WR_Locations'] = All['WR_Locations'].map(lambda x: ','.join(sorted(x.split(','))))

        All['Locations'] = All['tar_Location'].astype(str) + "," + All['Location1'].astype(str) 
        All['Locations'] = All['Locations'].map(lambda x: ','.join(sorted(x.split(','))))

        All['X_Depth_levels'] = All['tar_X_Depth_level'] + "," + All['X_Depth_level1']
        All['X_Depth_levels'] = All['X_Depth_levels'].map(lambda x: ','.join(sorted(x.split(','))))

        All['Y_Num'] = All['tar_Y_Numbers'] + "," + All['Y_Numbers1']
        All['Y_Num'] = All['Y_Num'].map(lambda x: ','.join(sorted(x.split(','))))

        All['Target_num'] = np.where(All['Routes'].str.split(',').str[0] == All['tar_route'],"tar1","tar2")


        Pos = ['TE,WR', 'RB,WR', 'RB,TE', 'WR,WR', 'TE,TE']

        use_routes = ['FLAT,HITCH', 'DIG,FLAT', 'HITCH,HITCH', 'DIG,HITCH', 'FLAT,SLANT', 'DRAG,FLAT', 'FLAT,OUT', 'GO,HITCH', 'HITCH,OUT', 'GO,OUT', 'DIG,OUT', 'DIG,DRAG', 'DEEPCROSS,FLAT', 'HITCH,SLANT', 'SLANT,SLANT', 'DRAG,OUT', 
                      'CORNER,FLAT', 'FLAT,GO', 'DRAG,HITCH', 'DEEPCROSS,GO', 'HITCH,OVERBALL', 'FLAT,OVERBALL','DIG,DIG', 'DIG,POST', 'DEEPCROSS,HITCH', 'HITCH,POST', 'DIG,SLANT', 'GO,GO', 'DIG,GO', 
                      'DEEPCROSS,DIG', 'FLAT,POST', 'OUT,OUT', 'CORNER,OUT', 'DEEPCROSS,OUT', 'GO,SLANT', 'CORNER,HITCH', 'BENEATH,DEEPCROSS', 'DEEPCROSS,POST', 'DRAG,GO', 'DRAG,DRAG', 'DEEPCROSS,DRAG', 'GO,POST',
                      'OUT,SLANT', 'OUT,POST', 'GO,WHIP', 'DRAG,POST', 'BENEATH,FLAT', 'CORNER,DEEPCROSS', 'DRAG,SLANT', 'DEEPCROSS,WHIP', 'OUT,OVERBALL', 'DIG,WHIP', 'CORNER,DRAG', 'OVERBALL,SLANT', 'DIG,OVERBALL', 
                      'CORNER,DIG', 'CORNER,WHIP', 'ANGLE,HITCH','OUT,WHIP', 'ANGLE,OUT', 'DRAG,WHEEL', 'DRAG,OVERBALL', 'COMEBACK,HITCH', 'FLAT,WHIP', 'HITCH,WHEEL']


        All = All.loc[(All['Routes'].isin(use_routes)) & (All['Positions'].isin(Pos))]

        return All

    def FindTriples(self,Coverages,Relative):
        One = Relative[['Year','week','gameId', 'playId', 'nflId', 'displayName', 'tar_pos','route', 'Target', 'TmDist_1', 'CT1', 'TmDist_2', 'CT2']]
        One.columns = ['Year','week','gameId', 'playId', 'tar_nflId', 'targetName', 'tar_pos', 'tar_route', 'Target', 'TmDist1', 'nflId1', 'TmDist2', 'nflId2']
        One['Closest_rank'] = 12

        Two = Relative[['Year','week','gameId', 'playId', 'nflId', 'displayName', 'tar_pos','route', 'Target', 'TmDist_1', 'CT1', 'TmDist_3', 'CT3']]
        Two.columns = ['Year','week','gameId', 'playId', 'tar_nflId', 'targetName', 'tar_pos', 'tar_route', 'Target', 'TmDist1', 'nflId1', 'TmDist2', 'nflId2']
        Two['Closest_rank'] = 13

        Three = Relative[['Year','week','gameId', 'playId', 'nflId', 'displayName', 'tar_pos','route', 'Target', 'TmDist_2', 'CT2', 'TmDist_3', 'CT3']]
        Three.columns = ['Year','week','gameId', 'playId', 'tar_nflId', 'targetName', 'tar_pos', 'tar_route', 'Target', 'TmDist1', 'nflId1', 'TmDist2', 'nflId2']
        Three['Closest_rank'] = 23

        All = pd.concat([One, Two, Three])
        All = All.loc[All['Target'] == 1].dropna(subset=['TmDist1','nflId1']).sort_values(by=['Year','week','gameId','playId'])
        All = All.loc[All['Target'] == 1].dropna(subset=['TmDist2','nflId2']).sort_values(by=['Year','week','gameId','playId'])

        Cols = ['gameId', 'playId','nflId','Target_Side_Route','PlayerTeam','OppTeam', 'Score_diff','quarter','down', 'yardsToGo','YardLine_std', 
                'gameClock','DropType2','RZ', 'Dist_Bin','Shotgun','RPO', 'Play_Action','Play_contains_motion', 'Motion_targetside', 'Motion_target','Target_on_farside', 
                'Target_on_weakside','Rollout', 'Rollout_farside', 'Rollout_weakside','Num_WRs', 'Num_TEs', 'Num_RBs', 'Total_Pos','Num_WRs_On_targetside', 'Num_TEs_On_targetside', 
                'Num_RBs_On_targetside', 'Players_On_Target_side', 'WR_Location','Location','X_Depth_level','Y_Numbers','epa','Success','NewCoverage']

        Details = Coverages[Cols]
        # Details.columns = ['gameId', 'playId','tar_nflId','Target_Side_Route','PlayerTeam','OppTeam','quarter', 'down', 'yardsToGo','YardLine_std', 'gameClock','epa','Success','NewCoverage']
        Details.columns = ['gameId', 'playId','tar_nflId','Target_Side_Route','PlayerTeam','OppTeam', 'Score_diff','quarter','down', 'yardsToGo','YardLine_std', 
                        'gameClock','DropType2','RZ', 'Dist_Bin','Shotgun','RPO', 'Play_Action','Play_contains_motion', 'Motion_targetside', 'Motion_target','Target_on_farside', 
                        'Target_on_weakside','Rollout', 'Rollout_farside', 'Rollout_weakside','Num_WRs', 'Num_TEs', 'Num_RBs', 'Total_Pos','Num_WRs_On_targetside', 'Num_TEs_On_targetside', 
                        'Num_RBs_On_targetside', 'Players_On_Target_side', 'tar_WR_Location','tar_Location','tar_X_Depth_level','tar_Y_Numbers','epa','Success','NewCoverage']
        Details = Details.set_index(['gameId','playId','tar_nflId'])
        All = All.set_index(['gameId','playId','tar_nflId']).join(Details).reset_index()

        for i in tqdm(range(1,3)):
            Cols = ['gameId', 'playId','nflId','position','route','On_Target_side', 'In_play','Side_route', 'WR_Location','Location','X_Depth_level','Y_Numbers']
            Match = Coverages[Cols]
            Match.columns = ['gameId', 'playId','nflId'+str(i),'position'+str(i),'route'+str(i),'On_Target_side'+str(i),'In_play'+str(i),'Side_route'+str(i),'WR_Location'+str(i),'Location'+str(i),'X_Depth_level'+str(i),'Y_Numbers'+str(i)]
            Match = Match.set_index(['gameId','playId','nflId'+str(i)])
            All = All.set_index(['gameId','playId','nflId'+str(i)]).join(Match).reset_index()

        All = All.loc[(All['In_play1'] == 1) & (All['In_play2'] == 1)]

        
        All['Positions'] = All['tar_pos'] + "," + All['position1'] + "," + All['position2']
        All['Positions'] = All['Positions'].map(lambda x: ','.join(sorted(x.split(','))))

        All['Routes'] = All['tar_route'] + "," + All['route1'] + "," + All['route2']
        All['Routes'] = All['Routes'].map(lambda x: ','.join(sorted(x.split(','))))

        All['Side_Routes'] = All['Target_Side_Route'] + "," + All['Side_route1'] + "," + All['Side_route2']
        All['Side_Routes'] = All['Side_Routes'].map(lambda x: ','.join(sorted(x.split(','))))

        All['WR_Locations'] = All['tar_WR_Location'].astype(str) + "," + All['WR_Location1'].astype(str) + "," + All['WR_Location2'].astype(str)
        All['WR_Locations'] = All['WR_Locations'].map(lambda x: ','.join(sorted(x.split(','))))

        All['Locations'] = All['tar_Location'].astype(str) + "," + All['Location1'].astype(str) + "," + All['Location2'].astype(str)
        All['Locations'] = All['Locations'].map(lambda x: ','.join(sorted(x.split(','))))

        All['X_Depth_levels'] = All['tar_X_Depth_level'] + "," + All['X_Depth_level1'] + "," + All['X_Depth_level2']
        All['X_Depth_levels'] = All['X_Depth_levels'].map(lambda x: ','.join(sorted(x.split(','))))

        All['Y_Num'] = All['tar_Y_Numbers'] + "," + All['Y_Numbers1'] + "," + All['Y_Numbers2']
        All['Y_Num'] = All['Y_Num'].map(lambda x: ','.join(sorted(x.split(','))))

        All['Target_num'] = np.where(All['Routes'].str.split(',').str[0] == All['tar_route'],"tar1","tar2")
        All['Target_num'] = np.where(All['Routes'].str.split(',').str[-1] == All['tar_route'],"tar3",All['Target_num'])

        Pos = ['RB,WR,WR', 'RB,TE,WR', 'TE,WR,WR', 'WR,WR,WR', 'TE,TE,WR', 'RB,TE,TE', 'RB,RB,WR', 'RB,RB,TE', 'TE,TE,TE']

        use_routes = ['FLAT,HITCH,HITCH', 'DIG,FLAT,HITCH', 'DIG,DRAG,FLAT', 'FLAT,HITCH,OVERBALL', 'FLAT,HITCH,SLANT','DIG,FLAT,OUT', 'HITCH,HITCH,HITCH', 'DRAG,FLAT,HITCH', 'FLAT,HITCH,OUT', 
                      'FLAT,SLANT,SLANT', 'DIG,HITCH,HITCH', 'DIG,DIG,FLAT', 'DIG,DRAG,OUT', 'GO,HITCH,HITCH', 'DIG,FLAT,POST', 'DRAG,FLAT,OUT', 'DIG,HITCH,OUT', 'DIG,FLAT,SLANT', 'CORNER,FLAT,HITCH', 'DEEPCROSS,FLAT,HITCH', 
                      'FLAT,GO,HITCH', 'DEEPCROSS,DIG,FLAT', 'DIG,DIG,HITCH', 'DEEPCROSS,FLAT,GO', 'DIG,HITCH,POST', 'FLAT,GO,OUT', 'HITCH,SLANT,SLANT', 'DRAG,DRAG,FLAT', 'DIG,GO,HITCH', 'FLAT,HITCH,POST', 'HITCH,HITCH,OVERBALL', 
                      'FLAT,OVERBALL,SLANT', 'HITCH,HITCH,OUT', 'DIG,FLAT,GO', 'DIG,DRAG,HITCH', 'CORNER,DEEPCROSS,FLAT', 'SLANT,SLANT,SLANT', 'GO,HITCH,OUT', 'DIG,GO,OUT', 'DEEPCROSS,DRAG,FLAT', 'DIG,DIG,OUT', 
                      'CORNER,DRAG,FLAT', 'DEEPCROSS,DIG,HITCH', 'HITCH,HITCH,SLANT', 'DEEPCROSS,FLAT,OUT', 'DIG,FLAT,OVERBALL', 'DIG,DRAG,GO', 'DRAG,GO,OUT', 'BENEATH,DEEPCROSS,FLAT', 'DIG,DRAG,POST', 'CORNER,DIG,FLAT', 
                      'DEEPCROSS,GO,OUT', 'DRAG,HITCH,OUT', 'DIG,DIG,DRAG', 'DIG,OUT,POST', 'HITCH,HITCH,POST', 'DEEPCROSS,GO,HITCH', 'DRAG,FLAT,SLANT', 'DIG,SLANT,SLANT', 'DIG,HITCH,SLANT', 'DEEPCROSS,FLAT,POST', 'DIG,DRAG,DRAG', 'DEEPCROSS,GO,GO', 'FLAT,OUT,POST', 'DEEPCROSS,DIG,DRAG','FLAT,OUT,SLANT', 'DEEPCROSS,DIG,GO']


        All = All.loc[(All['Routes'].isin(use_routes)) & (All['Positions'].isin(Pos))]

        return All

    def FindQuads(self,Coverages,Relative):
        One = Relative[['Year','week','gameId', 'playId', 'nflId', 'displayName', 'tar_pos','route', 'Target', 'TmDist_1', 'CT1', 'TmDist_2', 'CT2', 'TmDist_3', 'CT3']]
        One.columns = ['Year','week','gameId', 'playId', 'tar_nflId', 'targetName', 'tar_pos', 'tar_route', 'Target', 'TmDist1', 'nflId1', 'TmDist2', 'nflId2', 'TmDist3', 'nflId3']
        One['Closest_rank'] = 123

        Two = Relative[['Year','week','gameId', 'playId', 'nflId', 'displayName', 'tar_pos','route', 'Target', 'TmDist_1', 'CT1', 'TmDist_2', 'CT2', 'TmDist_4', 'CT4']]
        Two.columns = ['Year','week','gameId', 'playId', 'tar_nflId', 'targetName', 'tar_pos', 'tar_route', 'Target', 'TmDist1', 'nflId1', 'TmDist2', 'nflId2', 'TmDist3', 'nflId3']
        Two['Closest_rank'] = 124

        Three = Relative[['Year','week','gameId', 'playId', 'nflId', 'displayName', 'tar_pos','route', 'Target', 'TmDist_2', 'CT2', 'TmDist_3', 'CT3', 'TmDist_4', 'CT4']]
        Three.columns =  ['Year','week','gameId', 'playId', 'tar_nflId', 'targetName', 'tar_pos', 'tar_route', 'Target', 'TmDist1', 'nflId1', 'TmDist2', 'nflId2', 'TmDist3', 'nflId3']
        Three['Closest_rank'] = 234

        All = pd.concat([One, Two, Three])
        All = All.loc[All['Target'] == 1].dropna(subset=['TmDist1','nflId1']).sort_values(by=['Year','week','gameId','playId'])
        All = All.loc[All['Target'] == 1].dropna(subset=['TmDist2','nflId2']).sort_values(by=['Year','week','gameId','playId'])
        All = All.loc[All['Target'] == 1].dropna(subset=['TmDist3','nflId3']).sort_values(by=['Year','week','gameId','playId'])


        Cols = ['gameId', 'playId','nflId','Target_Side_Route','PlayerTeam','OppTeam', 'Score_diff','quarter','down', 'yardsToGo','YardLine_std', 
                'gameClock','DropType2','RZ', 'Dist_Bin','Shotgun','RPO', 'Play_Action','Play_contains_motion', 'Motion_targetside', 'Motion_target','Target_on_farside', 
                'Target_on_weakside','Rollout', 'Rollout_farside', 'Rollout_weakside','Num_WRs', 'Num_TEs', 'Num_RBs', 'Total_Pos','Num_WRs_On_targetside', 'Num_TEs_On_targetside', 
                'Num_RBs_On_targetside', 'Players_On_Target_side', 'WR_Location','Location','X_Depth_level','Y_Numbers','epa','Success','NewCoverage']

        Details = Coverages[Cols]
        # Details.columns = ['gameId', 'playId','tar_nflId','Target_Side_Route','PlayerTeam','OppTeam','quarter', 'down', 'yardsToGo','YardLine_std', 'gameClock','epa','Success','NewCoverage']
        Details.columns = ['gameId', 'playId','tar_nflId','Target_Side_Route','PlayerTeam','OppTeam', 'Score_diff','quarter','down', 'yardsToGo','YardLine_std', 
                        'gameClock','DropType2','RZ', 'Dist_Bin','Shotgun','RPO', 'Play_Action','Play_contains_motion', 'Motion_targetside', 'Motion_target','Target_on_farside', 
                        'Target_on_weakside','Rollout', 'Rollout_farside', 'Rollout_weakside','Num_WRs', 'Num_TEs', 'Num_RBs', 'Total_Pos','Num_WRs_On_targetside', 'Num_TEs_On_targetside', 
                        'Num_RBs_On_targetside', 'Players_On_Target_side', 'tar_WR_Location','tar_Location','tar_X_Depth_level','tar_Y_Numbers','epa','Success','NewCoverage']
        Details = Details.set_index(['gameId','playId','tar_nflId'])
        All = All.set_index(['gameId','playId','tar_nflId']).join(Details).reset_index()

        for i in tqdm(range(1,4)):
            Cols = ['gameId', 'playId','nflId','position','route','On_Target_side', 'In_play','Side_route', 'WR_Location','Location','X_Depth_level','Y_Numbers']
            Match = Coverages[Cols]
            Match.columns = ['gameId', 'playId','nflId'+str(i),'position'+str(i),'route'+str(i),'On_Target_side'+str(i),'In_play'+str(i),'Side_route'+str(i),'WR_Location'+str(i),'Location'+str(i),'X_Depth_level'+str(i),'Y_Numbers'+str(i)]
            Match = Match.set_index(['gameId','playId','nflId'+str(i)])
            All = All.set_index(['gameId','playId','nflId'+str(i)]).join(Match).reset_index()

        All = All.loc[(All['In_play1'] == 1) & (All['In_play2'] == 1) & (All['In_play3'] == 1)]

        All['Positions'] = All['tar_pos'] + "," + All['position1'] + "," + All['position2'] + "," + All['position3']
        All['Positions'] = All['Positions'].map(lambda x: ','.join(sorted(x.split(','))))

        All['Routes'] = All['tar_route'] + "," + All['route1'] + "," + All['route2'] + "," + All['route3']
        All['Routes'] = All['Routes'].map(lambda x: ','.join(sorted(x.split(','))))

        All['Side_Routes'] = All['Target_Side_Route'] + "," + All['Side_route1'] + "," + All['Side_route2']+ "," + All['Side_route3']
        All['Side_Routes'] = All['Side_Routes'].map(lambda x: ','.join(sorted(x.split(','))))

        All['WR_Locations'] = All['tar_WR_Location'].astype(str) + "," + All['WR_Location1'].astype(str) + "," + All['WR_Location2'].astype(str) + "," + All['WR_Location3'].astype(str)
        All['WR_Locations'] = All['WR_Locations'].map(lambda x: ','.join(sorted(x.split(','))))

        All['Locations'] = All['tar_Location'].astype(str) + "," + All['Location1'].astype(str) + "," + All['Location2'].astype(str) + "," + All['Location3'].astype(str)
        All['Locations'] = All['Locations'].map(lambda x: ','.join(sorted(x.split(','))))

        All['X_Depth_levels'] = All['tar_X_Depth_level'] + "," + All['X_Depth_level1'] + "," + All['X_Depth_level2']+ "," + All['X_Depth_level3']
        All['X_Depth_levels'] = All['X_Depth_levels'].map(lambda x: ','.join(sorted(x.split(','))))

        All['Y_Num'] = All['tar_Y_Numbers'] + "," + All['Y_Numbers1'] + "," + All['Y_Numbers2']+ "," + All['Y_Numbers3']
        All['Y_Num'] = All['Y_Num'].map(lambda x: ','.join(sorted(x.split(','))))

        All['Target_num'] = np.where(All['Routes'].str.split(',').str[0] == All['tar_route'],"tar1","tar3")
        All['Target_num'] = np.where(All['Routes'].str.split(',').str[1] == All['tar_route'],"tar2",All['Target_num'])
        All['Target_num'] = np.where(All['Routes'].str.split(',').str[-1] == All['tar_route'],"tar4",All['Target_num'])

        Pos = ['RB,TE,WR,WR', 'RB,RB,TE,WR', 'TE,WR,WR,WR', 'RB,WR,WR,WR', 'RB,TE,TE,WR', 'TE,TE,WR,WR', 'RB,RB,WR,WR', 'WR,WR,WR,WR', 'TE,TE,TE,WR', 'RB,TE,TE,TE', 'RB,RB,TE,TE']

        use_routes = ['DIG,DRAG,FLAT,OUT', 'DIG,DRAG,DRAG,FLAT', 'DIG,FLAT,HITCH,HITCH', 'DIG,FLAT,HITCH,OUT', 'DIG,DRAG,FLAT,GO', 'DIG,FLAT,GO,HITCH', 'DIG,FLAT,HITCH,POST', 
                      'FLAT,HITCH,HITCH,OVERBALL', 'DIG,DRAG,FLAT,POST', 'DEEPCROSS,DIG,FLAT,GO', 'DIG,FLAT,FLAT,HITCH', 'FLAT,HITCH,HITCH,SLANT', 'DEEPCROSS,DIG,DRAG,FLAT', 
                      'FLAT,FLAT,HITCH,HITCH', 'FLAT,HITCH,HITCH,OUT', 'DEEPCROSS,FLAT,GO,HITCH', 'DIG,HITCH,HITCH,POST', 'DIG,DIG,FLAT,HITCH', 
                      'DRAG,FLAT,GO,HITCH', 'DRAG,FLAT,HITCH,OUT', 'DEEPCROSS,FLAT,GO,GO', 'DIG,FLAT,FLAT,POST', 'DIG,DRAG,FLAT,HITCH']

        All = All.loc[(All['Routes'].isin(use_routes)) & (All['Positions'].isin(Pos))]

        return All