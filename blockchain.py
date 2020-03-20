# -*- coding: utf-8 -*-
"""
Created on Mon Dec 15 07:43:05 2019

@author: Oleksandr
"""

import hashlib
import datetime

supply = 20000
class Block:
    def __init__(self, timeStamp, transact, previous = ''):
        self.timeStamp = timeStamp
        self.transact = transact #refer here to get the info about transaction (wallet, amount)
        self.previous = previous
        self.difficultyIncrement = 0 #PoW
        self.hash = self.calculateHash(transact, timeStamp, self.difficultyIncrement) 

    def calculateHash(self, data, timeStamp, difficultyIncrement):
        data = str(data) + str(timeStamp) + str(difficultyIncrement) #input values for the hashing 
        data = data.encode()
        hash = hashlib.sha256(data) #hash the input using sha256
        return hash.hexdigest()

    def mineBlock(self,difficulty):
        PoW = "0" * difficulty #we want the hash of our block to start with the amount of 0s determined by difficulty variable
        while self.hash[:difficulty] != PoW:
            self.hash = self.calculateHash(self.transact,self.timeStamp,self.difficultyIncrement)
            self.difficultyIncrement = self.difficultyIncrement + 1 

class Blockchain:
    def __init__(self):
        self.chain = [self.construct_genesis()]
        self.difficulty = 5
        self.unconfirmed_transaction = []
        #miner reward is a function of supply and difficulty, when blockchain becomes long (100 blocks in our case) the reward halves
        if len(self.chain) < 100:
            self.reward = supply/(self.difficulty*100)
        elif len(self.chain) >= 100:
            self.reward = supply/(self.difficulty*200)
        elif len(self.chain) >= 200:
            self.reward = supply/(self.difficulty*400)
#Simple first block to iniciate the blockchain !attention! doesn't contain any transaction data that's why loop return an error                
    def construct_genesis(self):
        genesis = Block("23.11.1997","The Big Bang") 
        return genesis
#use the hash of the last block as a data inout for the new one
    def LastBlock(self):
        return self.chain[len(self.chain) - 1]

    def mine(self,minerReward):
        newBlock = Block(str(datetime.datetime.now()),self.unconfirmed_transaction)        
        newBlock.mineBlock(self.difficulty)
        newBlock.previous= self.LastBlock().hash
        self.chain.append(newBlock)
        print("Solution: " + newBlock.hash)
        print("Block added")
        rewardTrans = Transfer("Chain",minerReward,self.reward) #Miner reward works just like a normal transaction
        self.unconfirmed_transaction.append(rewardTrans)
        self.unconfirmed_transaction = []

#whether the hash of new block contains the correct hash of the previous block; to avoid remining and changing the data
    def Validity(self):
        for item in range(1,len(self.chain)):
            currentBlock = self.chain[item]
            previous= self.chain[item-1]
            if (currentBlock.previous != previous.hash):
                return ("Sabotage!")
        return ("The Chain is valid")

    def createTransfer(self,transaction):
        if (transaction.amount<100):
            self.unconfirmed_transaction.append(transaction)
        elif (transaction.amount>=100):
            return ("Transaction amount is too high")

#We loop through the whole chain and collect each transaction sent or recieved by the person 
    def getBalance(self,request):
        balance = 0
        for block in self.chain:
            if block.previous == "" : #avoid the first block
                continue 
            for transaction in block.transact:
                if transaction.sender == request:
                    balance -= transaction.amount
                if transaction.receiver == request:
                    balance += transaction.amount
        return balance

#Same idea as in previous function: collect all the senders - append into a list, same procedure with the receivers; match them. Mediocre loop, change   
    def wallets(self):
        allwallets = []
        senders = []
        receivers = []
        for block in self.chain:
            if block.previous == "":  #avoid the first block
                continue 
            for transaction in block.transact:
                senders.append(transaction.sender)
                if senders not in allwallets:
                    allwallets.append(senders)
                    receivers.append(transaction.receiver)
                    if receivers not in allwallets:
                        allwallets.append(receivers)
            return allwallets 
    
    def circulation(self):
        balances_combined = []
        balance_i = 0 
        i = 0
        while i < len(Transfer.walletnames):
            for block in self.chain:
                if block.previous == "" :
                    continue 
            for transaction in block.transact:
                if transaction.sender== i:
                    balance_i -= transaction.amount
                if transaction.receiver == i:
                    balance_i += transaction.amount
                    balances_combined.append(balance_i)
                    i += 1
        return balances_combined 

class Transfer:
    def __init__(self,sender,receiver,amount):
        self.sender = sender
        self.receiver = receiver
        self.amount = amount



name = Blockchain()
name.createTransfer(Transfer("Isabell","Sasha",3))
name.createTransfer(Transfer("Sonya","Sasha", 18.5))

print("Mining in process")
name.mine("Pedram")

name.createTransfer(Transfer("Sarah","Daniel",0.1))
name.createTransfer(Transfer("Ludovic","Darya",100))
name.createTransfer(Transfer("Pedram","Sasha",0.0000001))

print("Mining in process")
name.mine("Pedram")

print("Pedram owns " + str(name.getBalance("Pedram")) + " coins")
print("Sasha owns " + str(name.getBalance("Sasha")) + " coins")
print(name.Validity())
print(name.wallets())
