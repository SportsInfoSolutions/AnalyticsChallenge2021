#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jun 28 22:02:58 2021

@author: niczacharis
"""

import pandas as pd
import numpy as np
import seaborn as sns
import re
import matplotlib.pyplot as plt


#provided challenge data from github
gameInfoCsv = "https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2021/main/Data/GameInfo.csv"
playByPlayCsv = "https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2021/main/Data/PlayByPlay.csv"
playerTotalPointsCsv = "https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2021/main/Data/PlayerTotalPoints.csv"
skillPositionPlayersCsv = "https://raw.githubusercontent.com/SportsInfoSolutions/AnalyticsChallenge2021/main/Data/SkillPositionPlayers.csv"

#read above csv files into pandas dataframes
df_gameInfo = pd.read_csv(gameInfoCsv)
df_playByPlay = pd.read_csv(playByPlayCsv)
df_playerTotalPoints = pd.read_csv(playerTotalPointsCsv)
df_SkillPositionPlayers = pd.read_csv(skillPositionPlayersCsv)


#define route names
df_SkillPositionPlayers.loc[df_SkillPositionPlayers.Route.str.find('Flat') != -1, 'Route'] = 'Flat'
df_SkillPositionPlayers.loc[df_SkillPositionPlayers.Route.str.find('Swing') != -1, 'Route'] = 'Swing'
df_SkillPositionPlayers.loc[df_SkillPositionPlayers.Route.str.find('Chip - Curl') != -1, 'Route'] = 'Curl'
df_SkillPositionPlayers.loc[df_SkillPositionPlayers.Route.str.find('Chip - Drag') != -1, 'Route'] = 'Drag'
df_SkillPositionPlayers.loc[df_SkillPositionPlayers.Route.str.find('Chip - Seam') != -1, 'Route'] = 'Seam'
df_SkillPositionPlayers.loc[df_SkillPositionPlayers.Route.str.find('Screen - Bubble') != -1, 'Route'] = 'WR Screen'
df_SkillPositionPlayers.loc[df_SkillPositionPlayers.Route.str.find('Screen - Beneath') != -1, 'Route'] = 'WR Screen'
df_SkillPositionPlayers.loc[df_SkillPositionPlayers.Route.str.find('Screen - Tunnel') != -1, 'Route'] = 'WR Screen'
df_SkillPositionPlayers.loc[df_SkillPositionPlayers.Route.str.find('Screen - Quick') != -1, 'Route'] = 'WR Screen'
df_SkillPositionPlayers.loc[df_SkillPositionPlayers.Route.str.find('Screen - Drag') != -1, 'Route'] = 'WR Screen'
df_SkillPositionPlayers.loc[df_SkillPositionPlayers.Route.str.find('Screen - TE') != -1, 'Route'] = 'TE Screen'
df_SkillPositionPlayers.loc[df_SkillPositionPlayers.Route.str.find('Screen - RB') != -1, 'Route'] = 'RB Screen'
df_SkillPositionPlayers.loc[df_SkillPositionPlayers.Route.str.find('Check & Release') != -1, 'Route'] = 'Check Down'



#get the count of each route type and store in a dictionary
playsByRouteCombo = {}
grouped = df_SkillPositionPlayers.groupby(['GameID', 'EventID', 'SideOfCenter'])
for name, group in grouped:
    combo = [np.nan, np.nan]
    for index, row in group.iterrows():
        if df_SkillPositionPlayers.at[index, 'Order_OutsideToInside'] == 1:
            combo[0] = (df_SkillPositionPlayers.at[index, 'Route'])
        elif df_SkillPositionPlayers.at[index, 'Order_OutsideToInside'] == 2:
            combo[1] = (df_SkillPositionPlayers.at[index, 'Route'])
    if np.nan in combo:
        continue   
    combo = 'WR1: ' + combo[0] + '\nWR2: ' + combo[1]
    if combo not in playsByRouteCombo.keys():
        playsByRouteCombo[combo] = 1
    else:
        playsByRouteCombo[combo] += 1       
        
        
#convert dictionary to dataframe        
df_playsByRouteCombo = pd.DataFrame.from_dict(playsByRouteCombo, orient='index')
df_playsByRouteCombo = df_playsByRouteCombo.rename(columns={'index':'Combo', 0:'NumPlays'})
df_playsByRouteCombo.sort_values(by=['NumPlays'], axis=0, inplace=True, ascending=False)

#filter out outlier route combos (less than 100 plays) or (greater than 5000 plays)
df_playsByRouteCombo.loc[df_playsByRouteCombo['NumPlays'] < 100, 'NumPlays'] = np.nan
df_playsByRouteCombo.loc[df_playsByRouteCombo['NumPlays'] > 1000, 'NumPlays'] = np.nan
df_playsByRouteCombo = df_playsByRouteCombo.dropna()
df_playsByRouteCombo = df_playsByRouteCombo.head(20)

#plot results
plt.plot()
plt.figure(figsize=(7,7))
plt.subplots_adjust(bottom=0.25,left=.2)
ax1 = sns.heatmap(df_playsByRouteCombo.head(20), annot=True, fmt=".0f", cmap="RdYlGn", 
                  linewidths=2, cbar=False, xticklabels=True, yticklabels=True)
plt.title('Most Popular Route Combinations')
plt.savefig('MostPopularRoutes.pdf')


#for route in df_SkillPositionPlayers['Route']:
    #if route not in playsByRouteCombo.keys():
       # playsByRouteCombo[route] = 1
   # else:
       # playsByRouteCombo[route] += 1
       

print("\nPlay route counts computed")

########################

#join dfs together to get target and reception data together with play by play data
df_joined = pd.merge(df_playByPlay, df_SkillPositionPlayers,  how='inner', left_on=['GameID','EventID'], right_on = ['GameID','EventID'])

#get the number of targets and recpetions on non-throwAways per route grouped by coverage scheme
targetsCoverages = {}
receptionsCoverages = {}
grouped = df_joined.groupby(['GameID', 'EventID', 'SideOfCenter'])
for name, group in grouped:
    
    combo = [np.nan, np.nan]
    for index, row in group.iterrows():
        if df_joined.at[index, 'Order_OutsideToInside'] == 1:
            combo[0] = (df_SkillPositionPlayers.at[index, 'Route'])
        elif df_joined.at[index, 'Order_OutsideToInside'] == 2:
            combo[1] = (df_joined.at[index, 'Route'])
    if np.nan in combo:
        continue   
    combo = 'WR1: ' + combo[0] + '\nWR2: ' + combo[1]
    if combo not in df_playsByRouteCombo.index.values:
        continue
    
    
    for index, row in group.iterrows():
        if group.at[index, 'Order_OutsideToInside'] == 1 or group.at[index, 'Order_OutsideToInside'] == 2:
            if group.at[index, 'ThrowAway'] == 0:
                if group.at[index, 'Target'] == 1:
                    covScheme = group.at[index, 'CoverageScheme']
                    if covScheme not in targetsCoverages.keys():
                        targetsCoverages[covScheme] = {}
                        targetsCoverages[covScheme][combo] = 1
                    else:
                        if combo not in targetsCoverages[covScheme].keys():
                            targetsCoverages[covScheme][combo] = 1
                        else:
                            targetsCoverages[covScheme][combo] += 1
                if group.at[index, 'Reception'] == 1:
                    if covScheme not in receptionsCoverages.keys():
                        receptionsCoverages[covScheme] = {}
                        receptionsCoverages[covScheme][combo] = 1
                    else:
                        if combo not in receptionsCoverages[covScheme].keys():
                            receptionsCoverages[covScheme][combo] = 1
                        else:
                            receptionsCoverages[covScheme][combo] += 1

print("\nroute count by coverage scheme computed")

#create df that shows number of targets by route by coverage
df_targets = pd.DataFrame.from_dict({(i): targetsCoverages[i] 
                           for i in targetsCoverages.keys()},
                       orient='index')

#create df that shows number of receptions by route by coverage
df_receptions = pd.DataFrame.from_dict({(i): receptionsCoverages[i] 
                           for i in receptionsCoverages.keys()},
                       orient='index')


#filter out any routes that had less than 5 targets (not enough data points)
for column, row in df_targets.iteritems():
    df_targets.loc[df_targets[column] < 5, column] = np.nan
    
for column, row in df_receptions.iteritems():
    df_receptions.loc[df_receptions[column] < 5, column] = np.nan

#create completion percentage dataframe
df_tgRpJoined = df_receptions.div(df_targets) * 100


#get top 3 completion % routes by coverage scheme
topRoutesByCompPercentage = df_tgRpJoined.apply(lambda s, n: pd.Series(s.nlargest(n).index), axis=1, n=3)
topRoutesByCompPercentageValues = df_tgRpJoined.apply(lambda s, n: pd.Series((s.nlargest(n))), axis=1, n=3)


#plot results
plt.plot()
plt.figure(figsize=(14,7))
plt.subplots_adjust(bottom=0.25)
ax1 = sns.heatmap(topRoutesByCompPercentageValues, annot=True, fmt=".1f", cmap="RdYlGn", 
                  linewidths=2, cbar=False, xticklabels=True, yticklabels=True)
for t in ax1.texts: t.set_text(t.get_text() + " %")
plt.title('Completion % By Route Combo vs Coverage (Min. 5 Targets)')
plt.savefig('completion%.pdf') 

print("\ncompletion % by route/coverage scheme plotted")

########################

#get the total yardage per target on non-throwAways per route grouped by coverage scheme
yardsPerTargetCov = {}
for name, group in grouped:
    
    combo = [np.nan, np.nan]
    for index, row in group.iterrows():
        if df_joined.at[index, 'Order_OutsideToInside'] == 1:
            combo[0] = (df_SkillPositionPlayers.at[index, 'Route'])
        elif df_joined.at[index, 'Order_OutsideToInside'] == 2:
            combo[1] = (df_joined.at[index, 'Route'])
    if np.nan in combo:
        continue   
    combo = 'WR1: ' + combo[0] + '\nWR2: ' + combo[1]
    if combo not in df_playsByRouteCombo.index.values:
        continue
    
    
    
    for index, row in group.iterrows():
        if group.at[index, 'Order_OutsideToInside'] == 1 or group.at[index, 'Order_OutsideToInside'] == 2:
            if group.at[index, 'ThrowAway'] == 0:
                if group.at[index, 'Target'] == 1:
                    covScheme = group.at[index, 'CoverageScheme']
                    if covScheme not in yardsPerTargetCov.keys():
                        yardsPerTargetCov[covScheme] = {}
                        yardsPerTargetCov[covScheme][combo] = group.at[index, 'OffensiveYardage']
                    else:
                        if combo not in yardsPerTargetCov[covScheme].keys():
                            yardsPerTargetCov[covScheme][combo] = group.at[index, 'OffensiveYardage']
                        else:
                            yardsPerTargetCov[covScheme][combo] += group.at[index, 'OffensiveYardage']
                    
df_yardsPerRoute = pd.DataFrame.from_dict({(i): yardsPerTargetCov[i] 
                           for i in yardsPerTargetCov.keys() },
                       orient='index')

#create yardsPerTarget dataframe
df_yardsPerTarget = df_yardsPerRoute.div(df_targets)
df_yardsPerTarget.drop(labels = ['Combination', 'Other', 'Prevent'], inplace=True)

#get top 3 yards/target routes by coverage scheme
topRoutesByYardsPerTarget = df_yardsPerTarget.apply(lambda s, n: pd.Series(s.nlargest(n).index), axis=1, n=3)
topRoutesByYardsPerTargetValues = df_yardsPerTarget.apply(lambda s, n: pd.Series((s.nlargest(n).values)), axis=1, n=3)
topRoutesByYardsPerTarget.columns = ['1st', '2nd', '3rd']    
topRoutesByYardsPerTargetValues.columns = ['1st EPA/Target', '2nd EPA/Target', '3rd EPA/Target']  
topRoutesByYardsPerTargetValues = topRoutesByYardsPerTargetValues.round(2)


topRoutesByYardsPerTarget = pd.merge(left=topRoutesByYardsPerTarget, right=topRoutesByYardsPerTargetValues, how='outer', left_index=True, right_index=True)
topRoutesByYardsPerTarget = topRoutesByYardsPerTarget[['1st', '1st EPA/Target', '2nd','2nd EPA/Target', '3rd', '3rd EPA/Target']]

topRoutesByYardsPerTarget.to_csv('Top Routes By Yards per Target.csv')

#plot results
plt.figure(figsize=(14,7))
plt.subplots_adjust(bottom=0.25)
ax2 = sns.heatmap(topRoutesByYardsPerTargetValues, annot=True, fmt=".1f", cmap="RdYlGn", 
                  linewidths=2, cbar=False, xticklabels=True, yticklabels=True)
plt.title('Yards/Target By Route Combo vs Coverage (Min. 5 Targets)')
plt.savefig('yardsPerTarget.pdf') 

print("\nyards/target by route/coverage scheme plotted")


########################

#get the total yardage per route run and total routes run grouped by coverage scheme
routesRun = {}
yardsPerReception = {}
for name, group in grouped:
    
    combo = [np.nan, np.nan]
    for index, row in group.iterrows():
        if df_joined.at[index, 'Order_OutsideToInside'] == 1:
            combo[0] = (df_SkillPositionPlayers.at[index, 'Route'])
        elif df_joined.at[index, 'Order_OutsideToInside'] == 2:
            combo[1] = (df_joined.at[index, 'Route'])
    if np.nan in combo:
        continue   
    combo = 'WR1: ' + combo[0] + '\nWR2: ' + combo[1]
    if combo not in df_playsByRouteCombo.index.values:
        continue
    
    
    
    for index, row in group.iterrows():
        if group.at[index, 'Order_OutsideToInside'] == 1 or group.at[index, 'Order_OutsideToInside'] == 2:
            covScheme = group.at[index, 'CoverageScheme']
            if covScheme not in routesRun.keys():
                            routesRun[covScheme] = {}
                            routesRun[covScheme][combo] = 1
            else:
                if combo not in routesRun[covScheme].keys():
                    routesRun[covScheme][combo] = 1
                else:
                    routesRun[covScheme][combo] += 1
            if group.at[index, 'Reception'] == 1:
                if covScheme not in yardsPerReception.keys():
                    yardsPerReception[covScheme] = {}
                    yardsPerReception[covScheme][combo] = group.at[index, 'OffensiveYardage']
                else:
                    if combo not in yardsPerReception[covScheme].keys():
                        yardsPerReception[covScheme][combo] = group.at[index, 'OffensiveYardage']
                    else:
                        yardsPerReception[covScheme][combo] += group.at[index, 'OffensiveYardage']
    
    
#create routesRun and yardsPerReception dataframes from above dictionaries
df_routesRun = pd.DataFrame.from_dict({(i): routesRun[i] 
                           for i in routesRun.keys() },
                       orient='index')
for column, row in df_routesRun.iteritems():
    df_routesRun.loc[df_routesRun[column] < 30, column] = np.nan
    
df_yardsPerReception = pd.DataFrame.from_dict({(i): yardsPerReception[i] 
                           for i in yardsPerReception.keys() },
                       orient='index')

#create yardsPerRouteRun dataframe
df_yardsPerRouteRun = df_yardsPerReception.div(df_routesRun)
df_yardsPerRouteRun.drop(labels = [np.nan, 'Combination', 'Other', 'Prevent', 'Spike'], inplace=True)



#get top 3 yards/routeRun routes by coverage scheme
topRoutesByYardsPerRouteRun = df_yardsPerRouteRun.apply(lambda s, n: pd.Series(s.nlargest(n).index), axis=1, n=3)
topRoutesByYardsPerRouteRunValues = df_yardsPerRouteRun.apply(lambda s, n: pd.Series((s.nlargest(n).values)), axis=1, n=3)
topRoutesByYardsPerRouteRun.columns = ['1st', '2nd', '3rd']    
topRoutesByYardsPerRouteRunValues.columns = ['1st Yards/RouteRun', '2nd Yards/RouteRun', '3rd Yards/RouteRun']  
topRoutesByYardsPerRouteRunValues = topRoutesByYardsPerRouteRunValues.round(2)


topRoutesByYardsPerRouteRun = pd.merge(left=topRoutesByYardsPerRouteRun, right=topRoutesByYardsPerRouteRunValues, how='outer', left_index=True, right_index=True)
topRoutesByYardsPerRouteRun = topRoutesByYardsPerRouteRun[['1st', '1st Yards/RouteRun', '2nd','2nd Yards/RouteRun', '3rd', '3rd Yards/RouteRun']]

topRoutesByYardsPerRouteRun.to_csv('Top Routes By Yards per Route Run.csv')

#plot results
plt.figure(figsize=(14,7))
plt.subplots_adjust(bottom=0.25)
ax2 = sns.heatmap(topRoutesByYardsPerRouteRunValues, annot=True, fmt=".1f", cmap="RdYlGn", 
                  linewidths=2, cbar=False, xticklabels=True, yticklabels=True)
plt.title('Yards/RouteRun By Route Combo vs Coverage (Min. 30 Routes Run)')
plt.savefig('yardsPerRouteRun.pdf') 

print("\nyards/route run by route/coverage scheme plotted")

########################

#get the epa for all targets grouped by coverage scheme
epaPerTarget = {}
for name, group in grouped:
    
    combo = [np.nan, np.nan]
    for index, row in group.iterrows():
        if df_joined.at[index, 'Order_OutsideToInside'] == 1:
            combo[0] = (df_SkillPositionPlayers.at[index, 'Route'])
        elif df_joined.at[index, 'Order_OutsideToInside'] == 2:
            combo[1] = (df_joined.at[index, 'Route'])
    if np.nan in combo:
        continue   
    combo = 'WR1: ' + combo[0] + '\nWR2: ' + combo[1]
    if combo not in df_playsByRouteCombo.index.values:
        continue
    
    
    for index, row in group.iterrows():
        if group.at[index, 'Order_OutsideToInside'] == 1 or group.at[index, 'Order_OutsideToInside'] == 2:
            if group.at[index, 'ThrowAway'] == 0:
                if group.at[index, 'Target'] == 1:
                    covScheme = group.at[index, 'CoverageScheme']
                    if covScheme not in epaPerTarget.keys():
                        epaPerTarget[covScheme] = {}
                        epaPerTarget[covScheme][combo] = group.at[index, 'EPA']
                    else:
                        if combo not in epaPerTarget[covScheme].keys():
                            epaPerTarget[covScheme][combo] = group.at[index, 'EPA']
                        else:
                            epaPerTarget[covScheme][combo] += group.at[index, 'EPA']


df_epaByTargets = pd.DataFrame.from_dict({(i): epaPerTarget[i] 
                           for i in epaPerTarget.keys() },
                       orient='index')

#create epaPerTarget dataframe
df_epaPerTarget = df_epaByTargets.div(df_targets)
df_epaPerTarget.drop(labels = ['Combination', 'Other', 'Prevent'], inplace=True)


#get top 3 epa/target routes by coverage scheme
topRoutesByepaPerTarget = df_epaPerTarget.apply(lambda s, n: pd.Series(s.nlargest(n).index), axis=1, n=3)
topRoutesByepaPerTargetValues = df_epaPerTarget.apply(lambda s, n: pd.Series((s.nlargest(n).values)), axis=1, n=3)
topRoutesByepaPerTarget.columns = ['1st', '2nd', '3rd']    
topRoutesByepaPerTargetValues.columns = ['1st EPA/Target', '2nd EPA/Target', '3rd EPA/Target']  
topRoutesByepaPerTargetValues = topRoutesByepaPerTargetValues.round(2)


topRoutesByepaPerTarget = pd.merge(left=topRoutesByepaPerTarget, right=topRoutesByepaPerTargetValues, how='outer', left_index=True, right_index=True)
topRoutesByepaPerTarget = topRoutesByepaPerTarget[['1st', '1st EPA/Target', '2nd','2nd EPA/Target', '3rd', '3rd EPA/Target']]

topRoutesByepaPerTarget.to_csv('Top Routes By EPA per Target.csv')

#plot results
plt.figure(figsize=(14,7))
plt.subplots_adjust(bottom=0.25)
ax2 = sns.heatmap(topRoutesByepaPerTargetValues, annot=True, fmt=".2f", cmap="RdYlGn", 
                  linewidths=2, cbar=False, xticklabels=True, yticklabels=True)
plt.title('EPA/Target By Route Combo vs Coverage (Min. 5 Targets)')
plt.savefig('epaPerTarget.png') 

print("\nepa/target by route/coverage scheme plotted")

########################

#get the TD count for all targets grouped by coverage scheme
tdCount = {}
for name, group in grouped:
    
    combo = [np.nan, np.nan]
    for index, row in group.iterrows():
        if df_joined.at[index, 'Order_OutsideToInside'] == 1:
            combo[0] = (df_SkillPositionPlayers.at[index, 'Route'])
        elif df_joined.at[index, 'Order_OutsideToInside'] == 2:
            combo[1] = (df_joined.at[index, 'Route'])
    if np.nan in combo:
        continue   
    combo = 'WR1: ' + combo[0] + '\nWR2: ' + combo[1]
    if combo not in df_playsByRouteCombo.index.values:
        continue
    
    
    
    for index, row in group.iterrows():
        if group.at[index, 'Order_OutsideToInside'] == 1 or group.at[index, 'Order_OutsideToInside'] == 2:
            if group.at[index, 'ThrowAway'] == 0:
                if group.at[index, 'Target'] == 1:
                    if group.at[index, 'Touchdown'] == 1:
                        covScheme = group.at[index, 'CoverageScheme']
                        if covScheme not in tdCount.keys():
                            tdCount[covScheme] = {}
                            tdCount[covScheme][combo] = 1
                        else:
                            if combo not in tdCount[covScheme].keys():
                                tdCount[covScheme][combo] = 1
                            else:
                                tdCount[covScheme][combo] += 1

df_tdCount = pd.DataFrame.from_dict({(i): tdCount[i] 
                           for i in tdCount.keys() },
                       orient='index')

#create tdPerReception dataframe
df_tdPerReception = df_tdCount.div(df_receptions) * 100
df_tdPerReception.drop(labels = ['Combination', 'Other', 'Prevent'], inplace=True)



#get top 3 TD% routes by coverage scheme
topRoutesByTDPerReception = df_tdPerReception.apply(lambda s, n: pd.Series(s.nlargest(n).index), axis=1, n=3)
topRoutesByTDPerReceptionValues = df_tdPerReception.apply(lambda s, n: pd.Series((s.nlargest(n).values)), axis=1, n=3)
topRoutesByTDPerReception.columns = ['1st', '2nd', '3rd']    
topRoutesByTDPerReceptionValues.columns = ['1st TD %', '2nd TD %', '3rd TD %']  
topRoutesByTDPerReceptionValues = topRoutesByTDPerReceptionValues.round(1)

topRoutesByTDPerReception = pd.merge(left=topRoutesByTDPerReception, right=topRoutesByTDPerReceptionValues, how='outer', left_index=True, right_index=True)
topRoutesByTDPerReception = topRoutesByTDPerReception[['1st', '1st TD %', '2nd','2nd TD %', '3rd', '3rd TD %']]

topRoutesByTDPerReception.to_csv('Top Routes By TD Rate.csv')

#plot results
plt.figure(figsize=(14,7))
plt.subplots_adjust(bottom=0.19)
ax2 = sns.heatmap(topRoutesByTDPerReceptionValues, annot=True, fmt=".1f", cmap="RdYlGn", 
                  linewidths=2, cbar=False, xticklabels=True, yticklabels=True)
for t in ax2.texts: t.set_text(t.get_text() + " %")
plt.title('TD % By Route Combo vs Coverage (Min. 5 Receptions)')
plt.savefig('TD%.pdf') 

print("\nTD % by route/coverage scheme plotted")

########################

#get the 1st Down count for all targets grouped by coverage scheme
firstDownCount = {}
for name, group in grouped:
    
    combo = [np.nan, np.nan]
    for index, row in group.iterrows():
        if df_joined.at[index, 'Order_OutsideToInside'] == 1:
            combo[0] = (df_SkillPositionPlayers.at[index, 'Route'])
        elif df_joined.at[index, 'Order_OutsideToInside'] == 2:
            combo[1] = (df_joined.at[index, 'Route'])
    if np.nan in combo:
        continue   
    combo = 'WR1: ' + combo[0] + '\nWR2: ' + combo[1]
    if combo not in df_playsByRouteCombo.index.values:
        continue
    
    
    
    for index, row in group.iterrows():
        if group.at[index, 'Order_OutsideToInside'] == 1 or group.at[index, 'Order_OutsideToInside'] == 2:
            if group.at[index, 'ThrowAway'] == 0:
                if group.at[index, 'Target'] == 1:
                    if group.at[index, 'FirstDown'] == 1:
                        covScheme = group.at[index, 'CoverageScheme']
                        if covScheme not in firstDownCount.keys():
                            firstDownCount[covScheme] = {}
                            firstDownCount[covScheme][combo] = 1
                        else:
                            if combo not in firstDownCount[covScheme].keys():
                                firstDownCount[covScheme][combo] = 1
                            else:
                                firstDownCount[covScheme][combo] += 1

df_firstDownCount = pd.DataFrame.from_dict({(i): firstDownCount[i] 
                           for i in firstDownCount.keys() },
                       orient='index')

#create firstDownPerReception dataframe
df_firstDownPerReception = df_firstDownCount.div(df_receptions) * 100
df_firstDownPerReception.drop(labels = ['Combination', 'Other', 'Prevent'], inplace=True)


#get top 3 1st% routes by coverage scheme
topRoutesBy1stPerReception = df_firstDownPerReception.apply(lambda s, n: pd.Series(s.nlargest(n).index), axis=1, n=3)
topRoutesBy1stPerReceptionValues = df_firstDownPerReception.apply(lambda s, n: pd.Series((s.nlargest(n).values)), axis=1, n=3)
topRoutesBy1stPerReception.columns = ['1st', '2nd', '3rd']    
topRoutesBy1stPerReceptionValues.columns = ['1st 1st %', '2nd 1st %', '3rd 1st %']  
topRoutesBy1stPerReceptionValues = topRoutesBy1stPerReceptionValues.round(1)

topRoutesBy1stPerReception = pd.merge(left=topRoutesBy1stPerReception, right=topRoutesBy1stPerReceptionValues, how='outer', left_index=True, right_index=True)
topRoutesBy1stPerReception = topRoutesBy1stPerReception[['1st', '1st 1st %', '2nd','2nd 1st %', '3rd', '3rd 1st %']]

topRoutesBy1stPerReception.to_csv('Top Routes By 1st Rate.csv')

#plot results
plt.figure(figsize=(14,7))
plt.subplots_adjust(bottom=0.25)
ax2 = sns.heatmap(topRoutesBy1stPerReceptionValues, annot=True, fmt=".1f", cmap="RdYlGn", 
                  linewidths=2, cbar=False, xticklabels=True, yticklabels=True)
for t in ax2.texts: t.set_text(t.get_text() + " %")
plt.title('1st Down % By Route Combo vs Coverage (Min. 5 Receptions)')
plt.savefig('1st%.pdf') 

print("\n1st Down % by route/coverage scheme plotted")

########################

#get the INT count for all targets grouped by coverage scheme
intCount = {}
for name, group in grouped:
    
    combo = [np.nan, np.nan]
    for index, row in group.iterrows():
        if df_joined.at[index, 'Order_OutsideToInside'] == 1:
            combo[0] = (df_SkillPositionPlayers.at[index, 'Route'])
        elif df_joined.at[index, 'Order_OutsideToInside'] == 2:
            combo[1] = (df_joined.at[index, 'Route'])
    if np.nan in combo:
        continue   
    combo = 'WR1: ' + combo[0] + '\nWR2: ' + combo[1]
    if combo not in df_playsByRouteCombo.index.values:
        continue
    
    
    for index, row in group.iterrows():
        if group.at[index, 'Order_OutsideToInside'] == 1 or group.at[index, 'Order_OutsideToInside'] == 2:
            if group.at[index, 'ThrowAway'] == 0:
                if group.at[index, 'Target'] == 1:
                    covScheme = group.at[index, 'CoverageScheme']
                    if covScheme not in intCount.keys():
                            intCount[covScheme] = {}
                            intCount[covScheme][combo] = 0
                    if group.at[index, 'InterceptionOnPlay'] == 1:
                        
                        if combo not in intCount[covScheme].keys():
                            intCount[covScheme][combo] = 1
                        else:
                            intCount[covScheme][combo] += 1

df_intCount = pd.DataFrame.from_dict({(i): intCount[i] 
                           for i in intCount.keys() },
                       orient='index')

#create tdPerReception dataframe
df_intPerTarget = df_intCount.div(df_targets) * 100
df_intPerTarget.drop(labels = ['Combination', 'Other', 'Prevent'], inplace=True)


#get top 3 INT% routes by coverage scheme
topRoutesByINTPerTarget = df_intPerTarget.apply(lambda s, n: pd.Series(s.nsmallest(n).index), axis=1, n=3)
topRoutesByINTPerTargetValues = df_intPerTarget.apply(lambda s, n: pd.Series((s.nsmallest(n).values)), axis=1, n=3)
topRoutesByINTPerTarget.columns = ['1st', '2nd', '3rd']    
topRoutesByINTPerTargetValues.columns = ['1st INT %', '2nd INT %', '3rd INT %']  
topRoutesByINTPerTargetValues = topRoutesByINTPerTargetValues.round(1)


topRoutesByINTPerTarget = pd.merge(left=topRoutesByINTPerTarget, right=topRoutesByINTPerTargetValues, how='outer', left_index=True, right_index=True)
topRoutesByINTPerTarget = topRoutesByINTPerTarget[['1st', '1st INT %', '2nd','2nd INT %', '3rd', '3rd INT %']]

topRoutesByINTPerTarget.to_csv('Top Routes By INT Rate.csv')

#plot results
plt.figure(figsize=(14,7))
plt.subplots_adjust(bottom=0.25)
ax2 = sns.heatmap(topRoutesByINTPerTargetValues, annot=True, fmt=".1f", cmap="RdYlGn_r", 
                  linewidths=2, cbar=False, xticklabels=True, yticklabels=True)
for t in ax2.texts: t.set_text(t.get_text() + " %")
plt.title('INT % By Route Combo vs Coverage (Min. 5 Targets)')
plt.savefig('INT%.pdf') 

print("\nINT % by route/coverage scheme plotted")

########################

#get the count of successful(EPA > 0) plays for all targets grouped by coverage scheme
successCount = {}
for name, group in grouped:
    
    combo = [np.nan, np.nan]
    for index, row in group.iterrows():
        if df_joined.at[index, 'Order_OutsideToInside'] == 1:
            combo[0] = (df_SkillPositionPlayers.at[index, 'Route'])
        elif df_joined.at[index, 'Order_OutsideToInside'] == 2:
            combo[1] = (df_joined.at[index, 'Route'])
    if np.nan in combo:
        continue   
    combo = 'WR1: ' + combo[0] + '\nWR2: ' + combo[1]
    if combo not in df_playsByRouteCombo.index.values:
        continue
    
    
    for index, row in group.iterrows():
        if group.at[index, 'Order_OutsideToInside'] == 1 or group.at[index, 'Order_OutsideToInside'] == 2:
            if group.at[index, 'ThrowAway'] == 0:
                if group.at[index, 'Target'] == 1:
                    if group.at[index, 'EPA'] > 0:
                        covScheme = group.at[index, 'CoverageScheme']
                        if covScheme not in successCount.keys():
                            successCount[covScheme] = {}
                            successCount[covScheme][combo] = 1
                        else:
                            if combo not in successCount[covScheme].keys():
                                successCount[covScheme][combo] = 1
                            else:
                                successCount[covScheme][combo] += 1


df_successRate = pd.DataFrame.from_dict({(i): successCount[i] 
                           for i in successCount.keys() },
                       orient='index')

#create successRate dataframe
df_successRate = df_successRate.div(df_targets) * 100
df_successRate.drop(labels = ['Combination', 'Other', 'Prevent'], inplace=True)

#get top 3 success routes by coverage scheme
topRoutesBySuccessRate = df_successRate.apply(lambda s, n: pd.Series(s.nlargest(n).index), axis=1, n=3)
topRoutesBySuccessRateValues = df_successRate.apply(lambda s, n: pd.Series((s.nlargest(n).values)), axis=1, n=3)
topRoutesBySuccessRate.columns = ['1st', '2nd', '3rd']    
topRoutesBySuccessRateValues.columns = ['1st SuccessRate', '2nd SuccessRate', '3rd SuccessRate']  
topRoutesBySuccessRateValues = topRoutesBySuccessRateValues.round(1)


topRoutesBySuccessRate = pd.merge(left=topRoutesBySuccessRate, right=topRoutesBySuccessRateValues, how='outer', left_index=True, right_index=True)
topRoutesBySuccessRate = topRoutesBySuccessRate[['1st', '1st SuccessRate', '2nd','2nd SuccessRate', '3rd', '3rd SuccessRate']]

topRoutesBySuccessRate.to_csv('Top Routes By Success Rate.csv')

#plot results
plt.figure(figsize=(8,5))
plt.subplots_adjust(bottom=0.20)
ax2 = sns.heatmap(topRoutesBySuccessRateValues, annot=True, fmt=".1f", cmap="RdYlGn", 
                  linewidths=2, cbar=False, xticklabels=True, yticklabels=True)
for t in ax2.texts: t.set_text(t.get_text() + " %")
plt.title('Success Rate By Route Combo vs Coverage (Min. 5 Targets)')
plt.savefig('successRate.png') 

print("\nsuccess rate by route/coverage scheme plotted")







scoreboard = {}


for index, row in topRoutesByYardsPerTarget.iterrows():
    if index not in scoreboard.keys():
        scoreboard[index] = {}
    
    if row[0] != np.nan and type(row[0]) == str and 'WR1' in row[0]:
        
        if row[0] not in scoreboard[index].keys():
            scoreboard[index][row[0]] = 3
        else:
            scoreboard[index][row[0]] += 3
    
    if row[1] != np.nan and type(row[1]) == str and 'WR1' in row[1]:
        
        if row[1] not in scoreboard[index].keys():
            scoreboard[index][row[1]] = 2
        else:
            scoreboard[index][row[1]] += 2
    
    if row[2] != np.nan and type(row[2]) == str and 'WR1' in row[2]:
        print(row[2])
        if row[2] not in scoreboard[index].keys():
            scoreboard[index][row[2]] = 1
        else:
            scoreboard[index][row[2]] += 1      
      
         

for index, row in topRoutesByYardsPerRouteRun.iterrows():
    if index not in scoreboard.keys():
        scoreboard[index] = {}
    
    if row[0] != np.nan and type(row[0]) == str and 'WR1' in row[0]:
        
        if row[0] not in scoreboard[index].keys():
            scoreboard[index][row[0]] = 3
        else:
            scoreboard[index][row[0]] += 3
    
    if row[1] != np.nan and type(row[1]) == str and 'WR1' in row[1]:
        
        if row[1] not in scoreboard[index].keys():
            scoreboard[index][row[1]] = 2
        else:
            scoreboard[index][row[1]] += 2
    
    if row[2] != np.nan and type(row[2]) == str and 'WR1' in row[2]:
        print(row[2])
        if row[2] not in scoreboard[index].keys():
            scoreboard[index][row[2]] = 1
        else:
            scoreboard[index][row[2]] += 1      
      
        

for index, row in topRoutesByepaPerTarget.iterrows():
    if index not in scoreboard.keys():
        scoreboard[index] = {}

    if row[0] != np.nan and type(row[0]) == str and 'WR1' in row[0]:
        
        if row[0] not in scoreboard[index].keys():
            scoreboard[index][row[0]] = 3
        else:
            scoreboard[index][row[0]] += 3
    
    if row[1] != np.nan and type(row[1]) == str and 'WR1' in row[1]:
        
        if row[1] not in scoreboard[index].keys():
            scoreboard[index][row[1]] = 2
        else:
            scoreboard[index][row[1]] += 2
    
    if row[2] != np.nan and type(row[2]) == str and 'WR1' in row[2]:
        print(row[2])
        if row[2] not in scoreboard[index].keys():
            scoreboard[index][row[2]] = 1
        else:
            scoreboard[index][row[2]] += 1      
      
        
    
for index, row in topRoutesByTDPerReception.iterrows():
    if index not in scoreboard.keys():
        scoreboard[index] = {}
    if row[0] != np.nan and type(row[0]) == str and 'WR1' in row[0]:
        
        if row[0] not in scoreboard[index].keys():
            scoreboard[index][row[0]] = 3
        else:
            scoreboard[index][row[0]] += 3
    
    if row[1] != np.nan and type(row[1]) == str and 'WR1' in row[1]:
        
        if row[1] not in scoreboard[index].keys():
            scoreboard[index][row[1]] = 2
        else:
            scoreboard[index][row[1]] += 2
    
    if row[2] != np.nan and type(row[2]) == str and 'WR1' in row[2]:
        print(row[2])
        if row[2] not in scoreboard[index].keys():
            scoreboard[index][row[2]] = 1
        else:
            scoreboard[index][row[2]] += 1      
      
        
    
    
for index, row in topRoutesBy1stPerReception.iterrows():
    if index not in scoreboard.keys():
        scoreboard[index] = {}
    
    if row[0] != np.nan and type(row[0]) == str and 'WR1' in row[0]:
        
        if row[0] not in scoreboard[index].keys():
            scoreboard[index][row[0]] = 3
        else:
            scoreboard[index][row[0]] += 3
    
    if row[1] != np.nan and type(row[1]) == str and 'WR1' in row[1]:
        
        if row[1] not in scoreboard[index].keys():
            scoreboard[index][row[1]] = 2
        else:
            scoreboard[index][row[1]] += 2
    
    if row[2] != np.nan and type(row[2]) == str and 'WR1' in row[2]:
        print(row[2])
        if row[2] not in scoreboard[index].keys():
            scoreboard[index][row[2]] = 1
        else:
            scoreboard[index][row[2]] += 1      
      
        

for index, row in topRoutesByINTPerTarget.iterrows():
    if index not in scoreboard.keys():
        scoreboard[index] = {}
    
    if row[0] != np.nan and type(row[0]) == str and 'WR1' in row[0]:
        
        if row[0] not in scoreboard[index].keys():
            scoreboard[index][row[0]] = 3
        else:
            scoreboard[index][row[0]] += 3
    
    if row[1] != np.nan and type(row[1]) == str and 'WR1' in row[1]:
        
        if row[1] not in scoreboard[index].keys():
            scoreboard[index][row[1]] = 2
        else:
            scoreboard[index][row[1]] += 2
    
    if row[2] != np.nan and type(row[2]) == str and 'WR1' in row[2]:
        print(row[2])
        if row[2] not in scoreboard[index].keys():
            scoreboard[index][row[2]] = 1
        else:
            scoreboard[index][row[2]] += 1      
      
        
        
for index, row in topRoutesBySuccessRate.iterrows():
    if index not in scoreboard.keys():
        scoreboard[index] = {}
        
    if row[0] != np.nan and type(row[0]) == str and 'WR1' in row[0]:
        
        if row[0] not in scoreboard[index].keys():
            scoreboard[index][row[0]] = 3
        else:
            scoreboard[index][row[0]] += 3
    
    if row[1] != np.nan and type(row[1]) == str and 'WR1' in row[1]:
        
        if row[1] not in scoreboard[index].keys():
            scoreboard[index][row[1]] = 2
        else:
            scoreboard[index][row[1]] += 2
    
    if row[2] != np.nan and type(row[2]) == str and 'WR1' in row[2]:
        print(row[2])
        if row[2] not in scoreboard[index].keys():
            scoreboard[index][row[2]] = 1
        else:
            scoreboard[index][row[2]] += 1      
      
        
       
        
       
        
        
df_scoreboard = pd.DataFrame.from_dict({(i): scoreboard[i] 
                           for i in scoreboard.keys() },
                       orient='index')
df_scoreboard = df_scoreboard.div(21) * 100


    
topRoutesByCoverage = df_scoreboard.apply(lambda s, n: pd.Series(s.nlargest(n).index), axis=1, n=3)
topRoutesByCoverageValues = df_scoreboard.apply(lambda s, n: pd.Series((s.nlargest(n).values)), axis=1, n=3)       
topRoutesByCoverage.columns = ['1st', '2nd', '3rd']    
topRoutesByCoverageValues.columns = ['1st Score', '2nd Score', '3rd Score']  

topRoutesByCoverage = pd.merge(left=topRoutesByCoverage, right=topRoutesByCoverageValues, how='outer', left_index=True, right_index=True)
topRoutesByCoverage = topRoutesByCoverage[['1st', '1st Score', '2nd','2nd Score', '3rd', '3rd Score']]
topRoutesByCoverage = topRoutesByCoverage.round(1)


plt.figure(figsize=(6,5))
plt.subplots_adjust(bottom=0.25, left=.2)
ax2 = sns.heatmap(topRoutesByCoverageValues, annot=True, cmap="RdYlGn", 
                  linewidths=2, cbar=False, xticklabels=True, yticklabels=True, robust=True)
plt.title('Top Route Scores By Coverage')
plt.savefig('Score.pdf') 

  

topRoutesByCoverage.to_csv('Top Routes By Coverage.csv')


