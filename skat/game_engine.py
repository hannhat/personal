import numpy as np
import random


CARDS = [('9', 'r'), ('10', 'r'), ('J', 'r'), ('Q', 'r'), ('K', 'r'), ('A', 'r'),
         ('9', 'b'), ('10', 'b'), ('J', 'b'), ('Q', 'b'), ('K', 'b'), ('A', 'b')]


def deal_cards():
    hands = CARDS
    random.shuffle(hands)

    hand_lst = [hands[x:x+4] for x in range(0, len(hands), 4)]

    return hand_lst


class Player:
    def __init__(hand, number):
        self.hand = hand
        self.points = 0
        self.player_number = number
        self.following_player_hand_info = None
        self.preceding_player_hand_info = None


class Game:
    def __init__(on_the_play='1'):
        hand_lst = deal_cards()
        self.player_dict = {'1' : self.player1, '2' : self.player2,
                            '3' : self.player3}
        self.history = np.array([])
        self.cards_played = []
        self.player1 = Player(hand_lst[0], 1)
        self.player2 = Player(hand_lst[1], 2)
        self.player3 = Player(hand_lst[2], 3)
        self.player_lst = [self.player1, self.player2, self.player3]
        self.turn_starter = on_the_play
        self.aggressor = on_the_play
        self.defenders = self.player_lst.remove(on_the_play)
        self.aggressor_points = self.player_dict[self.aggressor].points
        self.defender_points = self.player_dict[self.defenders[0]].points \
                               + self.player_dict[self.defenders[1]].points
            
