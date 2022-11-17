import numpy as np
import os
from pathlib import Path
import pandas as pd
from pybaseball import standings

import json

import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

abspath = os.path.abspath(os.getcwd())
finpath = Path(abspath).resolve().parent.parent


class Tournament():
    '''
    '''
    def __init__(self, teams_lst, rounds={"DS" : 5, "LCS" : 7, "WS" : 7}):
        self.teams_lst = teams_lst
        self.placing_dict = {}
        self.rounds = rounds

    def simulate_series(self, teams, function, year, num_games):
        team1_wins = 0
        team2_wins = 0
        while (team1_wins < num_games / 2) and (team2_wins < num_games / 2):
            winner = function(*teams, year)
            if winner == teams[0]:
                team1_wins += 1
            else:
                team2_wins += 1
        if team1_wins > team2_wins:
            return teams[0], teams[1]
        else:
            return teams[1], teams[0]

    def simulate_round(self, teams_lst, function, year, round):
        winners = []
        losers = []
        teams = []
        for team in teams_lst:
            teams.append(team)
            if len(teams) == 2:
                winner, loser = self.simulate_series(teams, function, year, self.rounds[round])
                winners.append(winner)
                losers.append(loser)
                teams = []
        return winners, losers

    def simulate_playoffs(self, function, year):
        if len(self.teams_lst) == 8:
            round = 'DS'
        elif len(self.teams_lst) == 4:
            round = 'LCS'
        elif len(self.teams_lst) == 2:
            round = 'WS'
        teams_remaining = self.teams_lst
        while len(teams_remaining) > 1:
            winners, losers = self.simulate_round(teams_remaining, function, year, round)
            self.placing_dict["Lost in " + round] = losers
            if len(teams_remaining) == 2:
                self.placing_dict["Won WS"] = winners
            teams_remaining = winners
            if len(teams_remaining) == 8:
                round = 'DS'
            elif len(teams_remaining) == 4:
                round = 'LCS'
            elif len(teams_remaining) == 2:
                round = 'WS'


def create_placing_dict(master_dict, year, playoff_teams):
    master_dict[year] = {}
    for team in playoff_teams[year]:
        if len(playoff_teams[year]) == 8:
            master_dict[year][team] = {"Lost in DS" : 0, "Lost in LCS" : 0,
                                         "Lost in WS" : 0, "Won WS" : 0}
        elif len(playoff_teams[year]) == 4:
            master_dict[year][team] = {"Lost in LCS" : 0, "Lost in WS" : 0,
                                         "Won WS" : 0}
    return master_dict

def update_placing_dict(master_dict, placing_dict, year, trials):
    for placing, teams in placing_dict.items():
        for team in teams:
            master_dict[year][team][placing] += 1 / trials
    return master_dict


def playoff_sim(trials, func, year_start=1970, year_end=2019):
    master_dict = {}
    count = 0
    for _ in range(trials):
        for year in range(year_start, year_end + 1):
            if year != 1994:
                teams_lst = playoff_teams[year]
                bracket = Tournament(teams_lst)
                bracket.simulate_playoffs(func, year)
                if count == 0:
                    master_dict = create_placing_dict(master_dict, year, playoff_teams)
                master_dict = update_placing_dict(master_dict, bracket.placing_dict, year, trials)
        if count == 0:
            count = 1
    return master_dict

def score_simulation(master_dict, playoff_bracket, year_start=1970, year_end=2019, cost=1):
    scores = []
    outcomes_dict = {"Lost in DS" : 1, "Lost in LCS" : 2, "Lost in WS" : 3, "Won WS" : 4}
    results_dict = {}
    for year in range(year_start, year_end + 1):
        if year != 1994:
            score = 0
            results_dict = create_placing_dict(results_dict, year, playoff_teams)
            results_dict = update_placing_dict(results_dict, playoff_bracket[year], year, 1)
            for team in master_dict[year]:
                projections = master_dict[year][team]
                results = results_dict[year][team]
                outcome = list(results.keys())[list(results.values()).index(1)]
                for key in projections:
                    multiplier = np.abs(outcomes_dict[key] - outcomes_dict[outcome])
                    score += multiplier * (projections[key] ** cost)
            scores.append(score)
    return np.array(scores), np.sum(np.array(scores))

def write_simple_standings_to_df(year_start, year_end, output_file):
    '''
    Writes simple historical standings to a csv file. 
    '''
    data = []
    for year in range(year_start, year_end + 1):
        standing = standings(year)
        for division in standing:
            division["Year"] = year
            data.append(division)
    historical_standings = pd.concat(data, axis=0)
    historical_standings.to_csv(output_file, index=False)

def retrosheet_symbols_to_teams(output_file):
    teams = pd.read_csv(str(finpath) + '/sabermetrics_playground/storage/raw_data/retrosheet_team_abbreviations.csv')
    teams["Team Name"] = teams[" City"] + ' ' + teams[" Name"]
    teams = teams[["Abbreviation", "Team Name"]]
    teams = teams.set_index('Abbreviation')
    teams_dict = teams.to_dict()["Team Name"]
    with open(output_file, "w") as outfile:
        json.dump(teams_dict, outfile)