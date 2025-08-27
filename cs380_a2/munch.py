import util

DEFAULT_STATE = '---|---|---'


class Cell:
    EMPTY = '-'
    FORE = {'X': 'white', 'O': 'white', '-': 'white'}
    BACK = {'X': 'red', 'O': 'green', '-': 'black'}

    @classmethod
    def color(cls, char):
        return util.color_string(char, fore=Cell.FORE[char], back=Cell.BACK[char])


class Action:

    def __init__(self, char, x1, y1, x2, y2):
        self.char = char
        self.x1, self.y1 = x1, y1
        self.x2, self.y2 = x2, y2

    def __str__(self):
        return f'fill({self.char},{self.x1},{self.y1},{self.x2},{self.y2})'


class State:

    MAX_RECT = 2

    def __init__(self, string=None):
        string = string or DEFAULT_STATE
        self.board = [list(line) for line in string.split('|')]
        self.max_x = len(self.board[0]) if self.board else 0
        self.max_y = len(self.board)
        self.last_played = None

    def __str__(self):
        return '|'.join([''.join(row) for row in self.board])

    def __eq__(self, state):
        return str(self) == str(state)

    def clone(self):
        return State(str(self))

    def all_x(self):
        for x in range(self.max_x):
            yield x

    def all_y(self):
        for y in range(self.max_y):
            yield y

    def all_xy(self):
        for x in self.all_x():
            for y in self.all_y():
                yield x, y

    def all_xy_in(self, x1, y1, x2, y2):
        for x in range(x1, x2+1):
            for y in range(y1, y2+1):
                yield x, y

    def is_legal(self, x, y, x2=None, y2=None):
        return ((x >= 0 and y >= 0 and x < self.max_x and y < self.max_y)
                and ((x2 is None or y2 is None) or
                     (x2 >= 0 and y2 >= 0 and x2 < self.max_x and y2 < self.max_y)))

    def is_empty(self, x1, y1, x2, y2):
        for x, y in self.all_xy_in(x1, y1, x2, y2):
            if self.get(x, y) != Cell.EMPTY:
                return False
        return True

    def get(self, x, y):
        return self.board[y][x] if self.is_legal(x, y) else None

    def row(self, y):
        return self.board[y]

    def put(self, c, x, y):
        self.board[y][x] = c

    def fill(self, c, x1, y1, x2, y2):
        for x, y in self.all_xy_in(x1, y1, x2, y2):
            self.put(c, x, y)

    def actions(self, char):
        actions = []
        for x1, y1 in self.all_xy():
            for x2, y2 in self.all_xy_in(x1, y1, x1+self.MAX_RECT-1, y1+self.MAX_RECT-1):
                if self.is_legal(x1, y1, x2, y2):
                    if self.is_empty(x1, y1, x2, y2):
                        actions.append(Action(char, x1, y1, x2, y2))
        actions.sort(key=lambda action: str(action))
        return actions

    def execute(self, action):
        self.fill(action.char, action.x1, action.y1, action.x2, action.y2)
        self.last_played = action.char
        return self

    def game_over(self):
        empties = 0
        for x, y in self.all_xy():
            if self.get(x, y) == Cell.EMPTY:
                empties += 1
        return empties == 0

    def loser(self):
        return self.last_played if self.game_over() else None

    def pprint_string(self):
        return '\n'.join([
            ' ' + ''.join([Cell.color(c) for c in self.row(y)]) + ' '
            for y in self.all_y()
        ])


if __name__ == '__main__':
    cmd = util.get_arg(1)
    if cmd:
        if cmd == 'print':
            state = State(util.get_arg(2))
            util.pprint(state)
        elif cmd == 'over':
            state = State(util.get_arg(2))
            print(state.game_over())
        elif cmd == 'actions':
            char = util.get_arg(2)
            state = State(util.get_arg(3))
            for action in state.actions(char):
                print(action)
