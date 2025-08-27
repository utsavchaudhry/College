from munch import State
from util import pprint

class Player:

    def __init__(self, symbol):
        self.symbol = symbol

    def choose_action(self, state):
        raise NotImplementedError("This method should be overridden by subclasses.")

def get_legal_actions(state):
    actions = []
    rows = len(state)
    cols = len(state[0]) if rows > 0 else 0
    for i in range(rows):
        for j in range(cols):
            if state[i][j]:
                actions.append((i, j))
    return actions

def apply_action(state, action):
    if action is None:
        raise ValueError("No action provided to apply_action")
    r, c = action
    rows = len(state)
    cols = len(state[0]) if rows > 0 else 0
    
    new_state = [row[:] for row in state]
    # Remove the chosen piece and all pieces to its bottom-right
    for i in range(r, rows):
        for j in range(c, len(new_state[i])):
            new_state[i][j] = False
    return new_state

class Game:
    def __init__(self, state: State=None, player_x=None, player_o=None):
        self.state      = state or State()
        self.player_x   = player_x
        self.player_o   = player_o
        self.sequence   = []

    def play(self):
        turn = 'X'
        pprint(self.state)
        while not self.state.game_over():
            player = self.player_x if turn == 'X' else self.player_o
            action = player.choose_action(self.state)
            self.state.execute(action)
            print(f"{turn} plays {action}")
            pprint(self.state)
            self.sequence.append((turn, action))
            turn = 'O' if turn == 'X' else 'X'
        loser = self.state.loser()
        print(f"Game over! {loser} loses.")
        joined = ", ".join(f"{sym}:{act}" for sym, act in self.sequence)
        print("Move sequence:", joined)


