# multiAgents.py
# --------------
# Licensing Information:  You are free to use or extend these projects for
# educational purposes provided that (1) you do not distribute or publish
# solutions, (2) you retain this notice, and (3) you provide clear
# attribution to UC Berkeley, including a link to http://ai.berkeley.edu.
# 
# Attribution Information: The Pacman AI projects were developed at UC Berkeley.
# The core projects and autograders were primarily created by John DeNero
# (denero@cs.berkeley.edu) and Dan Klein (klein@cs.berkeley.edu).
# Student side autograding was added by Brad Miller, Nick Hay, and
# Pieter Abbeel (pabbeel@cs.berkeley.edu).


from util import manhattanDistance
from game import Directions
import random, util
import math

from game import Agent

class ReflexAgent(Agent):
    """
    A reflex agent chooses an action at each choice point by examining
    its alternatives via a state evaluation function.

    The code below is provided as a guide.  You are welcome to change
    it in any way you see fit, so long as you don't touch our method
    headers.
    """


    def getAction(self, gameState):
        """
        You do not need to change this method, but you're welcome to.

        getAction chooses among the best options according to the evaluation function.

        Just like in the previous project, getAction takes a GameState and returns
        some Directions.X for some X in the set {NORTH, SOUTH, WEST, EAST, STOP}
        """
        # Collect legal moves and successor states
        legalMoves = gameState.getLegalActions()

        # Choose one of the best actions
        scores = [self.evaluationFunction(gameState, action) for action in legalMoves]
        bestScore = max(scores)
        bestIndices = [index for index in range(len(scores)) if scores[index] == bestScore]
        chosenIndex = random.choice(bestIndices) # Pick randomly among the best

        "Add more of your code here if you want to"

        return legalMoves[chosenIndex]

    def evaluationFunction(self, currentGameState, action):
        """
        Design a better evaluation function here.

        The evaluation function takes in the current and proposed successor
        GameStates (pacman.py) and returns a number, where higher numbers are better.

        The code below extracts some useful information from the state, like the
        remaining food (newFood) and Pacman position after moving (newPos).
        newScaredTimes holds the number of moves that each ghost will remain
        scared because of Pacman having eaten a power pellet.

        Print out these variables to see what you're getting, then combine them
        to create a masterful evaluation function.
        """
        # Useful information you can extract from a GameState (pacman.py)
        successorGameState = currentGameState.generatePacmanSuccessor(action)
        newPos = successorGameState.getPacmanPosition()
        newFood = successorGameState.getFood()
        newGhostStates = successorGameState.getGhostStates()
        newScaredTimes = [ghostState.scaredTimer for ghostState in newGhostStates]
        newGhostPositions = [ghostState.getPosition() for ghostState in newGhostStates]
        
        newFoodDistances = [manhattanDistance(newPos, foodPos) for foodPos in newFood.asList()] 
        foodRisk = [[manhattanDistance(foodPos, ghostPos) for ghostPos in newGhostPositions] for foodPos in newFood.asList()]
        
        newFoodDistances = [foodDist - max(foodRisk[i]) for i, foodDist in enumerate(newFoodDistances)]

        foodHeuristic = 0 if len(newFoodDistances) == 0 else min(newFoodDistances)
        # ghostHeuristic = 0 if len(newFoodDistances) == 0 else min(newGhostDistances)

        inPlace = 9999 if currentGameState.getPacmanPosition() == newPos else 0
       
        "*** YOUR CODE HERE ***"
        return successorGameState.getScore() - foodHeuristic - inPlace

def scoreEvaluationFunction(currentGameState):
    """
    This default evaluation function just returns the score of the state.
    The score is the same one displayed in the Pacman GUI.

    This evaluation function is meant for use with adversarial search agents
    (not reflex agents).
    """
    return currentGameState.getScore()

class MultiAgentSearchAgent(Agent):
    """
    This class provides some common elements to all of your
    multi-agent searchers.  Any methods defined here will be available
    to the MinimaxPacmanAgent, AlphaBetaPacmanAgent & ExpectimaxPacmanAgent.

    You *do not* need to make any changes here, but you can if you want to
    add functionality to all your adversarial search agents.  Please do not
    remove anything, however.

    Note: this is an abstract class: one that should not be instantiated.  It's
    only partially specified, and designed to be extended.  Agent (game.py)
    is another abstract class.
    """

    def __init__(self, evalFn = 'scoreEvaluationFunction', depth = '2'):
        self.index = 0 # Pacman is always agent index 0
        self.evaluationFunction = util.lookup(evalFn, globals())
        self.depth = int(depth)

class MinimaxAgent(MultiAgentSearchAgent):
    """
    Your minimax agent (question 2)
    """

    def getAction(self, gameState):
        """
        Returns the minimax action from the current gameState using self.depth
        and self.evaluationFunction.

        Here are some method calls that might be useful when implementing minimax.

        gameState.getLegalActions(agentIndex):
        Returns a list of legal actions for an agent
        agentIndex=0 means Pacman, ghosts are >= 1

        gameState.generateSuccessor(agentIndex, action):
        Returns the successor game state after an agent takes an action

        gameState.getNumAgents():
        Returns the total number of agents in the game

        gameState.isWin():
        Returns whether or not the game state is a winning state

        gameState.isLose():
        Returns whether or not the game state is a losing state
        """
        "*** YOUR CODE HERE ***"
        # Collect legal moves and successor states
        legalMoves = gameState.getLegalActions(0)
        
        legalStates = [gameState.generateSuccessor(0, action) for action in legalMoves]
        scores = [self.minimax(state, self.depth, 1) for state in legalStates]
        bestScore = max(scores)
        bestIndices = [index for index in range(len(scores)) if scores[index] == bestScore]
        chosenIndex = random.choice(bestIndices) # Pick randomly among the best

        return legalMoves[chosenIndex]

    def minimax(self, gameState, currentDepth, currentAgent):
        if (currentDepth == 0):
            return self.evaluationFunction(gameState)
        elif (gameState.isWin() or gameState.isLose()):
            return self.evaluationFunction(gameState)
        else:
            legalStates = [gameState.generateSuccessor(currentAgent, action) for action in gameState.getLegalActions(currentAgent)]
    
            nextDepth = currentDepth - 1 if currentAgent == gameState.getNumAgents() - 1 else currentDepth
            nextAgent = 0 if currentAgent == gameState.getNumAgents() - 1 else currentAgent + 1
            
            scores = [self.minimax(state, nextDepth, nextAgent) for state in legalStates]
            bestScore = max(scores) if currentAgent == 0 else min(scores)
            bestIndices = [index for index in range(len(scores)) if scores[index] == bestScore]
            chosenIndex = random.choice(bestIndices) # Pick randomly among the best
            
            return scores[chosenIndex]
        

class AlphaBetaAgent(MultiAgentSearchAgent):
    """
    Your minimax agent with alpha-beta pruning (question 3)
    """

    def getAction(self, gameState):
        """
        Returns the minimax action using self.depth and self.evaluationFunction
        """
        "*** YOUR CODE HERE ***"
        legalActions = gameState.getLegalActions(0)
        
        alpha = -math.inf
        beta = math.inf
        
        scores = []
        for action in legalActions:
            state = gameState.generateSuccessor(0, action)
            score = self.minimax(state, self.depth, 1, alpha, beta)
            scores.append(score)            
            alpha = max(alpha, max(scores))
                
        bestScore = max(scores)
        bestIndices = [index for index in range(len(scores)) if scores[index] == bestScore]
        chosenIndex = random.choice(bestIndices) # Pick randomly among the best

        return legalActions[chosenIndex]

    def minimax(self, gameState, currentDepth, currentAgent, alpha, beta):
        if (currentDepth == 0):
            return self.evaluationFunction(gameState)
        elif (gameState.isWin() or gameState.isLose()):
            return self.evaluationFunction(gameState)
        else:
            legalActions = gameState.getLegalActions(currentAgent)
            
            nextDepth = currentDepth - 1 if currentAgent == gameState.getNumAgents() - 1 else currentDepth
            nextAgent = 0 if currentAgent == gameState.getNumAgents() - 1 else currentAgent + 1
            
            scores = []
            for action in legalActions:
                state = gameState.generateSuccessor(currentAgent, action)
                score = self.minimax(state, nextDepth, nextAgent, alpha, beta)
                scores.append(score)
                shouldBreak = score > beta if currentAgent == 0 else score < alpha
                if shouldBreak:
                    break
                elif currentAgent == 0:
                    alpha = max(alpha, max(scores))
                else:
                    beta = min(beta, min(scores))
            
            bestScore = max(scores) if currentAgent == 0 else min(scores)
            bestIndices = [index for index in range(len(scores)) if scores[index] == bestScore]
            chosenIndex = random.choice(bestIndices) # Pick randomly among the best
            
            return scores[chosenIndex]

class ExpectimaxAgent(MultiAgentSearchAgent):
    """
      Your expectimax agent (question 4)
    """

    def getAction(self, gameState):
        """
        Returns the minimax action from the current gameState using self.depth
        and self.evaluationFunction.

        Here are some method calls that might be useful when implementing minimax.

        gameState.getLegalActions(agentIndex):
        Returns a list of legal actions for an agent
        agentIndex=0 means Pacman, ghosts are >= 1

        gameState.generateSuccessor(agentIndex, action):
        Returns the successor game state after an agent takes an action

        gameState.getNumAgents():
        Returns the total number of agents in the game

        gameState.isWin():
        Returns whether or not the game state is a winning state

        gameState.isLose():
        Returns whether or not the game state is a losing state
        """
        "*** YOUR CODE HERE ***"
        # Collect legal moves and successor states
        legalMoves = gameState.getLegalActions(0)
        
        legalStates = [gameState.generateSuccessor(0, action) for action in legalMoves]
        scores = [self.minimax(state, self.depth, 1) for state in legalStates]
        bestScore = max(scores)
        bestIndices = [index for index in range(len(scores)) if scores[index] == bestScore]
        chosenIndex = random.choice(bestIndices) # Pick randomly among the best

        return legalMoves[chosenIndex]

    def minimax(self, gameState, currentDepth, currentAgent):
        if (currentDepth == 0):
            return self.evaluationFunction(gameState)
        elif (gameState.isWin() or gameState.isLose()):
            return self.evaluationFunction(gameState)
        else:
            legalStates = [gameState.generateSuccessor(currentAgent, action) for action in gameState.getLegalActions(currentAgent)]
    
            nextDepth = currentDepth - 1 if currentAgent == gameState.getNumAgents() - 1 else currentDepth
            nextAgent = 0 if currentAgent == gameState.getNumAgents() - 1 else currentAgent + 1
            
            scores = [self.minimax(state, nextDepth, nextAgent) for state in legalStates]
            if currentAgent == 0:            
                bestScore = max(scores)
                bestIndices = [index for index in range(len(scores)) if scores[index] == bestScore]
                chosenIndex = random.choice(bestIndices) # Pick randomly among the best
            
                return scores[chosenIndex]
            else:
                return sum(scores)/len(scores)


def betterEvaluationFunction(currentGameState):
    """
    Your extreme ghost-hunting, pellet-nabbing, food-gobbling, unstoppable
    evaluation function (question 5).

    DESCRIPTION: This uses the same evaluation function as before by 
    weighing the distance to food against how close that food is to a ghost
    """
    "*** YOUR CODE HERE ***"
    newPos = currentGameState.getPacmanPosition()
    newFood = currentGameState.getFood()
    newGhostStates = currentGameState.getGhostStates()
    newScaredTimes = [ghostState.scaredTimer for ghostState in newGhostStates]
    newGhostPositions = [ghostState.getPosition() for ghostState in newGhostStates]
    
    newFoodDistances = [manhattanDistance(newPos, foodPos) for foodPos in newFood.asList()] 
    foodRisk = [[manhattanDistance(foodPos, ghostPos) for ghostPos in newGhostPositions] for foodPos in newFood.asList()]
        
    newFoodDistances = [foodDist - max(foodRisk[i]) for i, foodDist in enumerate(newFoodDistances)]

    foodHeuristic = 0 if len(newFoodDistances) == 0 else min(newFoodDistances)
    # ghostHeuristic = 0 if len(newFoodDistances) == 0 else min(newGhostDistances)
    
    #inPlace = 9999 if currentGameState.getPacmanPosition() == newPos else 0
        
    return currentGameState.getScore() - foodHeuristic


# Abbreviation
better = betterEvaluationFunction
