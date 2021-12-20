# Johann Hatzius
# Jane Street December 2021 Puzzle

''' 
This problem utilizes Monte Carlo simulations to determine the probability of
player 4 winning. Justification for the exact Monte Carlo number used, as well
as the decision to model the competition as choosing a uniform random number
between 0 and 1 is provided in the written portion of my answer. 
'''

import math
import random

def monte_carlo():
    p4_wins = 0
    monte_carlo_number = 100000000

    for x in range(0, monte_carlo_number):
        competitors = ["p1", "p2", "p3", "p4"]
        min_dist = 1
        num_shots = 0
        while len(competitors) > 1:
            new_dist = random.random()
            if new_dist <= min_dist:
                min_dist = new_dist
                num_shots += 1
            else:
                rem_players = len(competitors)
                index = num_shots % rem_players #check
                del competitors[index] #check
                if (index + 1) > len(competitors):
                    num_shots = 0
                else:
                    num_shots = index
        winner = competitors[0]
        if winner == "p4":
            p4_wins += 1
    p4_win_probability = p4_wins / monte_carlo_number
    return p4_win_probability