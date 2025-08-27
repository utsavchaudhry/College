import random
from munch import State
from game import Player

class RandomPlayer(Player):
    def __init__(self, symbol='X'):
        self.symbol = symbol
        super().__init__(symbol)
        
    def choose_action(self, state: State):
        actions = state.actions(self.symbol)
        if not actions:
            return None
        return random.choice(actions)

class MinimaxPlayer(Player):
    def __init__(self, symbol='X'):
        super().__init__(symbol)

    def choose_action(self, state):
        best_score = float('-inf')
        alpha = float('-inf')
        beta  = float('inf')
        best_action = None
        for action in state.actions(self.symbol):
            child = state.clone().execute(action)
            score = self._alphabeta(child,
                                   'O' if self.symbol == 'X' else 'X',
                                   depth=1,
                                   alpha=alpha,
                                   beta=beta)
            if score > best_score:
                best_score, best_action = score, action
            alpha = max(alpha, best_score)
        return best_action

    def _alphabeta(self, state, current_symbol, depth, alpha, beta):
        if state.game_over() or depth >= 4:
            loser = state.loser()
            if loser is not None:
                return (-100 + depth) if loser == self.symbol else (100 - depth)
            # heuristics:
            my_moves  = len(state.actions(self.symbol))
            op_moves  = len(state.actions('O' if self.symbol == 'X' else 'X'))
            return my_moves - op_moves

        if current_symbol == self.symbol:
            value = float('-inf')
            for action in state.actions(current_symbol):
                child = state.clone().execute(action)
                value = max(value, self._alphabeta(child,
                                                   'O' if current_symbol == 'X' else 'X',
                                                   depth+1,
                                                   alpha, beta))
                alpha = max(alpha, value)
                if alpha >= beta:
                    break  # β cutoff
            return value
        else:
            value = float('inf')
            for action in state.actions(current_symbol):
                child = state.clone().execute(action)
                value = min(value, self._alphabeta(child,
                                                   'O' if current_symbol == 'X' else 'X',
                                                   depth+1,
                                                   alpha, beta))
                beta = min(beta, value)
                if beta <= alpha:
                    break  # α cutoff
            return value

    def minimax_value(self, state, current_symbol, depth):
        # +ve => favorable for self
        # -ve => favorable for opponent.

        if state.game_over():
            if current_symbol == self.symbol:
                return 100 - depth
            else:
                return -100 + depth
            
        actions = state.actions(current_symbol)

        if current_symbol == self.symbol:
            max_score = float('-inf')
            for action in actions:
                new_state = state.clone().execute(action)
                next_symbol = 'O' if current_symbol == 'X' else 'X'
                score = self.minimax_value(new_state, next_symbol, depth+1)
                if score > max_score:
                    max_score = score
            return max_score
        else:
            min_score = float('inf')
            for action in actions:
                new_state = state.clone().execute(action)
                next_symbol = 'O' if current_symbol == 'X' else 'X'
                score = self.minimax_value(new_state, next_symbol, depth+1)
                if score < min_score:
                    min_score = score
            return min_score
