
class Red():
    def __init__(self):
        self.red = "fire"
        Red.now = self.start
        self.nexT = None
        self.returnIt = None

    def start(self):
        self.red += "1"
        if len(self.red) < 10:
            self.nexT = self.state1
            return
        self.returnIt = ""
        self.nexT = None

    def state1(self):
        self.red += "2"
        print Red.now.__name__
        if True:
            self.nexT = self.start

red = Red()
while red.returnIt is None:
    red.__class__.now()
    red.__class__.now = red.nexT




