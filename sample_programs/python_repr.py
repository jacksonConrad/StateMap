def main():
	def state1():
	def state2():
	def state3():

def subdfa1():
	shit = 1;
	def state1():
		if(1<2):
			state2() 
	def state2():
		shit++
		if(shit<3):
			state1()

	def state3():
	
def __main__:
	return main()



#concurrent(dfa1,dfa2,dfa3)

class dfa1:
    def __init__(self):
        self.stackDum = list()
        self.now = self.start
        self.nexT = None
        self.returnIt = None
    def start(self):
        if len(self.stackDum) < 10:
            self.nexT = self.state2
        else:
            self.nexT = self.end

    def state2(self):
        self.stackDum.add(0)
        self.nexT = self.start
    
    def end(self):
        self.returnIt = len(self.stackDum)
        self.nexT = None

class dfa2:
    def __init__(self):
        self.stackDum = list()
        self.now = self.start
        self.nexT = None
        self.returnIt = None
    def start(self):
        if len(self.stackDum) < 10:
            self.nexT = self.state2
        else:
            self.nexT = self.end

    def state2(self):
        self.stackDum.add(0)
        self.nexT = self.start
    
    def end(self):
        self.returnIt = len(self.stackDum)
        self.nexT = None



class dfa3:
    def __init__(self):
        self.stackDum = list()
        self.now = self.start
        self.nexT = None
        self.returnIt = None
    def start(self):
        if len(self.stackDum) < 10:
            self.nexT = self.state2
        else:
            self.nexT = self.end

    def state2(self):
        self.stackDum.add(0)
        self.nexT = self.start
    
    def end(self):
        self.returnIt = len(self.stackDum)
        self.nexT = None


class main:
    def __init__(self,args1,args2,args3):
        self.args1 = args1#alt style
        self.args2 = args2#alt style
        self.args = [args1,args2,args3] #have this, even if blank
        self.stack1 = range(33)
        self.stack2 = list()
        self.now = self.start
        self.nexT = None
        self.returnIt = None
        while not returnIt: #NOTE: THIS IS ONLY TRUE FOR MAIN DO NOT DO FOR OTHER DFAS
            self.now()
            self.now = self.nexT()

   def start(self):
       #run stuff
       if 1 < 2:
           self.nexT = self.state2
       else:
           self.nexT = self.quit
   
    def quit(self):
        self.returnIt = "fuckAll"
        self.nexT = None

    def state2(self):
        self.stack2.add(self.stack1().pop(0))
        if len(self.stack2) < 10:
            self.nexT = self.start
        else:
            #concurrent(dfa1,dfa2,dfa2)
            dfasToRun = [dfa1(), dfa2(), dfa3()]
            while not all(dfa.returnIt is None for dfa in dfasToRun): 
                for dfa in dfasToRun:
                    dfa.now()
                for dfa in dfasToRun:
                    dfa.now = dfa.nexT

            self.nexT = self.quit

    def state3(self):
        dfasToRun = [dfa2()]
        #set up like concurrency but simpler
        while not all(dfa.returnIt is None for dfa in dfasToRun):
            for dfa in dfasToRun:
                dfa.now()
            for dfa in dfasToRun:
                dfa.now = dfa.nexT
        
        return "moreThanFuckAll" + arg




