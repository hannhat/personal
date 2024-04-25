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
import os
import json

path = os.path.abspath(os.getcwd())


def build_csv_individuals(start_year, end_year, min_pa):
    pitcher_data = pitching_stats(start_year, end_year, qual=min_pa)
    pitcher_data = pitcher_data.sort_values(by=['Season'], ascending=False)
    batter_data = batting_stats(start_year, end_year, qual=min_pa)
    batter_data = batter_data.sort_values(by=['Season'], ascending=False)
    pitcher_data.to_csv(path + '/storage/fangraphs/pitching', index=False)
    batter_data.to_csv(path + '/storage/fangraphs/batting', index=False)

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

