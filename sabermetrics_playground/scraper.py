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

path = os.path.abspath(os.getcwd())

def build_csv_individuals(start_year, end_year):
    pitcher_data = pitching_stats(start_year, end_year)
    pitcher_data = pitcher_data.sort_values(by=['Season'], ascending=False)
    #batter_data = batting_stats(start_year, end_year)
    #batter_data = batter_data.sort_values(by=['Season'], ascending=False)
    pitcher_data.to_csv(path + '/storage/fangraphs/pitching')
    #batter_data.to_csv(path + '/storage/fangraphs/batting')


if __name__ == '__main__':
    build_csv_individuals(2000, 2021)