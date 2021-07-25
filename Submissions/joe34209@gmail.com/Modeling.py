import pandas as pd
import numpy as np
from catboost import CatBoostRegressor, CatBoostClassifier
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import LabelEncoder
import random
from sklearn.model_selection import train_test_split

from sklearn.model_selection import KFold,StratifiedKFold
from sklearn.model_selection import train_test_split
from catboost import CatBoostClassifier
from sklearn.model_selection import StratifiedKFold,GridSearchCV, RandomizedSearchCV,cross_val_score

from sklearn.preprocessing import MinMaxScaler
from sklearn.preprocessing import StandardScaler
import random
import pickle
from sklearn.metrics import classification_report,auc,roc_auc_score,confusion_matrix,precision_recall_curve, make_scorer, recall_score, accuracy_score, precision_score,roc_curve,f1_score,log_loss,mean_squared_error,r2_score,median_absolute_error,mean_absolute_error



class InitiateModel():
    def __init__(self, Train=False) -> None:
        self.Train = Train
        pass

    def CreateData(self, df, var,route_combo):

        if route_combo == "ThreeMan":
            c2 = ['On_Target_side2','position2','route2','Location2','WR_Location2']
            int_c = ['On_Target_side2','WR_Location2']
        if route_combo == "FourMan":
            c2 = ['On_Target_side2','position2','route2','Location2','WR_Location2','On_Target_side3','position3','route3','Location3','WR_Location3']
            int_c = ['On_Target_side2','WR_Location2','On_Target_side3','WR_Location3']
        if route_combo == "TwoMan":
            c2 = []
            int_c = []
    #    Doubles_trim = df.loc[df['NewCoverage'] == cov]

        Doubles_trim = df.copy()

        Cols = ['NewCoverage','tar_route','down','Dist_Bin','Rollout','RZ', 'Target_on_farside', 'tar_pos', 'Shotgun','Play_Action','tar_WR_Location','tar_Location','On_Target_side1','position1','route1',
                'Location1', 'Routes', 'Positions','WR_Location1'] + c2


        Cols = list(set(Cols))
        
        X = Doubles_trim[Cols]
        y = Doubles_trim[var]

        X = X.fillna(-999)

        int_cols = ['down','RZ','tar_WR_Location','Dist_Bin','Rollout', 'Target_on_farside','Shotgun','Play_Action','On_Target_side1','WR_Location1'] + int_c

        for cat in int_cols:
            X[cat] = X[cat].astype('int')

        cat_cols = ['NewCoverage','tar_WR_Location','WR_Location1','tar_route','down','Dist_Bin','Rollout','RZ', 'Target_on_farside', 'tar_pos', 'Shotgun','Play_Action','tar_Location','On_Target_side1','position1',
                    'route1','Location1', 'Routes', 'Positions'] + c2


        for cat in cat_cols:
            X[cat] = X[cat].astype('category')
            

        # scale = StandardScaler()
        # X[['YardLine_std']] = scale.fit_transform(X[['YardLine_std']])

        # X['YardLine_std'] = X['YardLine_std'].astype(float)

        # pkl_filename = "Models/All/scaler.pkl"
        # with open(pkl_filename, 'wb') as file:
        #     pickle.dump(scale, file)
        
        categorical_features_indices = np.where(X.dtypes == 'category')[0]

        print(cat_cols)
        for cat in cat_cols:
            X[cat] = X[cat].astype(str)

        if var == "Success":
            X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=random.randint(1,100), shuffle=True, stratify=y)
        else:
            X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=random.randint(1,100), shuffle=True)

        return X_train, X_test, y_train, y_test, categorical_features_indices


    def Model(self, df, var, i, route_combo):
        X_train, X_test, y_train, y_test,categorical_features_indices = self.CreateData(df, var, route_combo)
        print(route_combo, var, i)
    #    print(var, i)
        if var == "Success":
            kfold = StratifiedKFold(n_splits=5, random_state=random.randint(1,100),shuffle=True)

            params = {'iterations': [200]
                    }
            model =  CatBoostClassifier(loss_function = 'Logloss',eval_metric='Logloss',task_type = 'GPU', od_type='Iter',early_stopping_rounds=50, logging_level='silent')
            grid = RandomizedSearchCV(model, params, cv=kfold)
            MLPclf = grid.fit(X_train, y_train,eval_set=(X_test, y_test), use_best_model=True, verbose=False,cat_features=categorical_features_indices)
            
            test_pred = MLPclf.best_estimator_.predict(X_test)
            test_prob = MLPclf.best_estimator_.predict_proba(X_test)[:,1]

            print("----------Test----------")
            print("Accuracy: {}".format(round(accuracy_score(y_test, test_pred),3)))
            print("log_loss: {}".format(round(log_loss(y_test, test_prob),3)))
            print("Recall: {}".format(recall_score(y_test, test_pred),3))
            print("roc_auc_score: {}".format(roc_auc_score(y_test, test_pred),3))
        else:
            kfold = KFold(n_splits=5, random_state=random.randint(1,100),shuffle=True)

            params = {'iterations': [200],}
            model =  CatBoostRegressor(loss_function = 'RMSE',eval_metric='RMSE',task_type = 'GPU', od_type='Iter',early_stopping_rounds=50, logging_level='silent')
            grid = RandomizedSearchCV(model, params, cv=kfold)
            MLPclf = grid.fit(X_train, y_train,eval_set=(X_test, y_test), use_best_model=True, verbose=False,cat_features=categorical_features_indices)
            test_pred = MLPclf.best_estimator_.predict(X_test)

            print("----------Test----------")
            print("RMSE: {}".format(np.sqrt(round(mean_squared_error(y_test, test_pred),3))))
            print("MAE: {}".format(round(mean_absolute_error(y_test, test_pred),3)))
            print("R2: {}".format(r2_score(y_test, test_pred),3))

        pkl_filename = "Models/All/"+ route_combo + "_" + var + "_" + str(i) + ".pkl"
        with open(pkl_filename, 'wb') as file:
            pickle.dump(MLPclf, file)
        return