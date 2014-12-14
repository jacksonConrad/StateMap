class EOS:
    red = "blue"
    def __init__(self):
        EOS.red = "red"
        return
    def __type__(self):
        return "EOS"

print EOS.red
