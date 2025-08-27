import sys
from munch   import State
from game    import Game
from human   import HumanPlayer
from agent   import RandomPlayer, MinimaxPlayer

def make_player(kind, symbol):
    if kind == 'human':   return HumanPlayer(symbol)
    if kind == 'random':  return RandomPlayer(symbol)
    if kind == 'minimax': return MinimaxPlayer(symbol)
    raise ValueError("Unknown player type")

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python main.py [human|random|minimax] [human|random|minimax]")
        sys.exit(1)
    p1 = make_player(sys.argv[1], 'X')
    p2 = make_player(sys.argv[2], 'O')
    game = Game(state=State(), player_x=p1, player_o=p2)
    game.play()
