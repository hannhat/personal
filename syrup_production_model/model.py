# Johann Hatzius
# Gro Homework

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_absolute_percentage_error
import xgboost as xgb
from datetime import datetime
from datetime import timedelta

pd.options.mode.chained_assignment = None


def find_weekly_sum(x, precip_df, temp_df, soil_df):
    row = ndvi.iloc[x]
    start = row["start_date"]
    end = row["end_date"]
    dates_precip = (precip_df["start_date"] >= start) & (precip_df["start_date"] <= end)
    dates_temp = (temp_df["start_date"] >= start) & (temp_df["start_date"] <= end)
    dates_soil = (soil_df["start_date"] >= start) & (soil_df["start_date"] <= end)
    precip_sum = precip_df.loc[dates_precip]["precip"].sum()
    mean_temp = temp_df.loc[dates_temp]["temp"].mean()
    mean_soil_moisture = soil_df.loc[dates_soil]["smos"].mean()
    new_cols = [precip_sum, mean_temp, mean_soil_moisture]
    return new_cols

def find_regions():
    regions = production["region_id"].tolist()
    return set(regions)

def build_features_df(precipitation, temperature, soil, ndvi):
    regions = find_regions()
    precip_col = []
    temp_col = []
    smos_col = []
    ndvi_col = []
    for region in regions:
        precip_df = precipitation[precipitation["region_id"] == region]
        temp_df = temperature[temperature["region_id"] == region]
        soil_df = soil[soil["region_id"] == region]
        ndvi_df = ndvi[ndvi["region_id"] == region]
        new_cols = [find_weekly_sum(x, precip_df, temp_df, soil_df) for x in ndvi_df.index]
        new_cols = normalize(new_cols)
        new_cols = np.array(new_cols).transpose()
        new_cols = new_cols.tolist()
        precip_col += new_cols[0]
        temp_col += new_cols[1]
        smos_col += new_cols[2]
        scaler = StandardScaler()
        ndvi_data = np.array(ndvi_df["ndvi"]).reshape(-1,1)
        scaler.fit(ndvi_data)
        ndvi_data = scaler.transform(ndvi_data)
        ndvi_col += list(np.array(ndvi_data).flatten())
    ndvi["precip"] = precip_col
    ndvi["temp"] = temp_col
    ndvi["smos"] = smos_col
    ndvi["ndvi"] = ndvi_col
    features_df = ndvi
    return features_df

def normalize(columns):
    scaler = StandardScaler()
    scaler.fit(columns)
    columns = scaler.transform(columns)
    return columns

def add_month_col(prod):
    prod["month"] = [int(x.split('-')[1]) for x in prod["start_date"]]
    return prod

def create_array_row(x, regional_df, precipitation, soil, temperature, ndvi):
    row = production.iloc[x]
    features = [precipitation, soil, temperature, ndvi]
    start_date = row["start_date"]
    date_int = datetime.strptime(start_date, "%Y-%m-%dT00:00:00.000Z")
    while regional_df["start_date"].isin([start_date]).any() == False:
        date_int = date_int - timedelta(days=1)
        start_date = datetime.strftime(date_int, "%Y-%m-%dT00:00:00.000Z")
        date_int = datetime.strptime(start_date, "%Y-%m-%dT00:00:00.000Z")
    regional_index = int(regional_df[regional_df["start_date"] == start_date].index.values[0])
    features_df = regional_df.loc[regional_index - 38:regional_index]
    features_df = features_df.drop(['start_date', 'end_date', 'region_id'], axis=1)
    features = features_df.to_numpy()
    features_t = features.flatten().tolist()
    final_row = row.tolist() + features_t
    return final_row
    
def create_master_array(features_df, production):
    prod = add_month_col(production)
    training_data_lst = []
    testing_data_lst = []
    regions = find_regions()
    for region in regions:
        regional_df = features_df[features_df["region_id"] == region]
        prod_df = prod[prod["region_id"] == region]
        region_list = [create_array_row(x, regional_df, precipitation, soil,
                      temperature, ndvi) for x in prod_df.index]
        region_data = np.array(region_list)
        training_data_lst.append(region_data[:-12, 2:])
        testing_data_lst.append(region_data[-12:, 2:])
    training_data = np.vstack(training_data_lst)
    training_data = pd.DataFrame(training_data).dropna(axis=0)
    training_data = np.array(training_data)
    testing_data = np.vstack(testing_data_lst)
    testing_data = pd.DataFrame(testing_data).dropna(axis=0)
    testing_data = np.array(testing_data)
    return (training_data, testing_data)

def create_target_array(features_df, predicted_production):
    predicted_production
    prod = add_month_col(predicted_production)
    target_data_lst = []
    regions = find_regions()
    for region in regions:
        regional_df = features_df[features_df["region_id"] == region]
        prod_df = prod[prod["region_id"] == region]
        region_list = [create_array_row(x, regional_df, precipitation, soil,
                      temperature, ndvi) for x in prod_df.index]
        region_data = pd.DataFrame(region_list)
        region_data = np.array(region_data)
        target_data_lst.append(region_data[:, 2:])
    target_data = np.vstack(target_data_lst)
    return target_data

def xgboost_model(training_data, testing_data, target_data, predicted_production):
    X_train, y_train = training_data[:, 1:], training_data[:, :1]
    X_test, y_test = testing_data[:, 1:], testing_data[:, :1]
    X_target = target_data[:, 1:]
    xg_reg = xgb.XGBRegressor(objective ='reg:squarederror', subsample=0.85, eta=0.2)
    xg_reg.fit(X_train,y_train)
    #test_preds = xg_reg.predict(X_test)
    #mape = mean_absolute_percentage_error(y_test, test_preds)
    #print("MAPE: %f" % (mape))
    preds = xg_reg.predict(X_target)
    predicted_production["prod"] = preds
    predicted_production.drop('month',axis=1)
    predicted_production.to_csv("jhatzius@uchicago.edu.csv")


if __name__ == '__main__':
    precipitation = pd.read_csv("Daily Precipitation.csv")
    temperature = pd.read_csv("Daily Temperature.csv")
    soil = pd.read_csv("Daily Soil Mositure.csv")
    ndvi = pd.read_csv("Eight Day NDVI.csv")
    production = pd.read_csv("Production Quantity.csv")
    predicted_production = pd.read_csv("predicted_production_qty.csv")

    features_df = build_features_df(precipitation, temperature, soil, ndvi)
    training_data, testing_data = create_master_array(features_df, production)
    target_data = create_target_array(features_df, predicted_production)
    xgboost_model(training_data, testing_data, target_data, predicted_production)
