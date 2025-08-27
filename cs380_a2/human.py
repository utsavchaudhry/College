from game import Player
from game import State

class HumanPlayer(Player):
    def __init__(self, symbol='X'):
        super().__init__(symbol)

    def choose_action(self, state: State):
        actions = state.actions(self.symbol)
        if not actions:
            raise RuntimeError("No legal actions available")
        
        print("Available actions:")
        for idx, action in enumerate(actions):
            print(f"  {idx}: {action}")
        
        choice = None
        while choice is None:
            try:
                user_input = input(f"Player {self.symbol}, choose an action (0-{len(actions)-1}): ")
            except EOFError:
                print("No input detected. Choosing first action by default.")
                return actions[0]
            if user_input is None:
                continue
            user_input = user_input.strip()
            if user_input.isdigit():
                idx = int(user_input)
                if 0 <= idx < len(actions):
                    choice = actions[idx]
                else:
                    print(f"Please enter a number between 0 and {len(actions)-1}.")
            else:
                print("Invalid input. Please enter a number.")
        return choice
