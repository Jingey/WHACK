class Wire:
    def __init__(self, funcs=list):
        self.funcs = funcs

    def callAll(self, data):
        for func in self.funcs:
            func(data)

        self.funcs = []

    def enlist(self, func):
        self.funcs.append(func)

    def delist(self, func):
        self.funcs.remove(func)

class Register:
    def __init__(self, data=list):
        self.data = data

    def set(self, data):
        tmp = self.data
        self.data = data

        return tmp
