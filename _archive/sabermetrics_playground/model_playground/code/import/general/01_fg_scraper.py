from pybaseball import statcast
from pybaseball import playerid_lookup
from pybaseball import statcast_pitcher
from pybaseball import statcast_batter
from pybaseball import pitching_stats
from pybaseball import batting_stats
from pybaseball import schedule_and_record
from pybaseball import standings
import numpy as np
import pandas as pd
from pathlib import Path 
import time

path = str(Path(__file__).resolve().parents[3])
print(path)

def fg_pitching_pull(start_year, end_year):
    pitcher_data = pitching_stats(start_year, end_year, qual=1)
    pitcher_data = pitcher_data.sort_values(by=['Season'], ascending=False)
    print(path)
    pitcher_data.to_csv(path + '/data/import/fangraphs/fg_pitching_' + str(start_year) + "_" + str(end_year) + '.csv', index=False)

def fg_batting_pull(start_year, end_year, min_pa):
    batter_data = batting_stats(start_year, end_year, qual=min_pa)
    batter_data = batter_data.sort_values(by=['Season'], ascending=False)
    batter_data.to_csv(path + '/data/import/general/fangraphs/fg_batting_' + str(start_year) + "_" + str(end_year) + '.csv', index=False)

def build_csv_schedule_and_record(start_year, end_year):
    print(path)
    q = pd.read_csv(path + '/fangraphs/batting')
    team_abbreviations = set(q[q["Season"] >= start_year]["Team"])
    print(team_abbreviations)
    dfs_lst = []
    for i in range(start_year, end_year + 1):
        for t in team_abbreviations:
            df = schedule_and_record(i, t)
            dfs_lst.append(df)
            print(df.head())
    print(dfs_lst) 
    game_data = pd.concat(dfs_lst)
    game_data.to_csv(path + 'fangraphs/game_data')

def pull_statcast_pitching(start_year, end_year):
    start_dt = str(start_year) + "-01-01"
    end_dt = str(end_year) + "-12-31"

    player_ids = pd.read_csv(path + '/data/import/statcast/season_level_pitching.csv')["player_id"]

    statcast_data = list()
    for id in player_ids:
        player_df = statcast_pitcher(start_dt, end_dt, id)
        statcast_data.append(player_df)
        print(len(statcast_data))
        time.sleep(1)
    statcast_df = pd.concat(statcast_data)
    statcast_df.to_csv("help.csv")

pull_statcast_pitching(2020, 2024)