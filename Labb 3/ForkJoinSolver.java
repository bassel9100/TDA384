package amazed.solver;

import amazed.maze.Maze;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.Stack;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver
    extends SequentialSolver
{
	// the number of step we have done since last fork
	private int stepSinceFork;
	// path from the start point to the current spawn point
	List<Integer> pathFromStart = new ArrayList<Integer>();
	// a shared boolean to stop the threads once the goal is found
	private AtomicBoolean goalFound = new AtomicBoolean();
	// a list of children of the parent thread
	private ArrayList<ForkJoinSolver> children;
	
    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
    public ForkJoinSolver(Maze maze)
    {
        super(maze);
        // set the fork after value on each crossing by default so we don't really need to provide this value
        this.forkAfter = 1;
        // Initialize the children list of the new instance
        this.children = new ArrayList<ForkJoinSolver>();
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze        the maze to be searched
     * @param forkAfter   the number of steps (visited nodes) after
     *                    which a parallel task is forked; if
     *                    <code>forkAfter &lt;= 0</code> the solver never
     *                    forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter)
    {
        this(maze);
        this.forkAfter = forkAfter;
    }
    
    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze       	the maze to be searched
     * @param forkAfter   	the number of steps (visited nodes) after
     *                    	which a parallel task is forked; if
     *                    	<code>forkAfter &lt;= 0</code> the solver never
     *                    	forks new tasks
     * @param visited	  	the shared Set of visited nodes
     * 					  	concurrently shared between children and parents
     * @param startPosition starting point of the child
     * @param pathFromStart path to the starting point of the first parent
     * @param goalFound		shared boolean to stop all the parents and children
     * 						at the same time when the goal is found                  
     */
    public ForkJoinSolver(Maze maze, int forkAfter, Set<Integer> visited, int startPosition, List<Integer> pathFromStart, AtomicBoolean goalFound)
    {
        this(maze, forkAfter);
        this.visited = visited;
        this.start = startPosition;
        this.pathFromStart = pathFromStart;
        this.goalFound = goalFound;
    }
    
    /**
     * Set of identifiers of all nodes in shared frontier
     */
    protected Set<Integer> sharedFrontier;
    
    /**
     * Initializes <code>visited</code>, <code>predecessor</code>, and
     * <code>frontier</code> with empty data structures for sequential
     * access.
     */
    @Override
    protected void initStructures()
    {
        visited = new ConcurrentSkipListSet<>();
        sharedFrontier = new ConcurrentSkipListSet<>();
        frontier = new Stack<>();
        predecessor = new HashMap<>();
    }

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */
    @Override
    public List<Integer> compute()
    {
        return parallelSearch();
    }

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */
    private List<Integer> parallelSearch()
    {
    	// because it is possible for the start node be visited after the instance creation
		if (!visited.contains(start)) {
			// one player active on the maze at start
	        int player = maze.newPlayer(start);
	        // start with start node
	        frontier.push(start);
	        // as long as not all nodes have been processed
	        while (!frontier.empty() && !this.goalFound.get()) {
	        	// get the new node to process
	            int current = frontier.pop();
	            // fork if needed with the rest of the frontier stack
	            if (this.stepSinceFork >= this.forkAfter) {
	            	// create the new solvers
	            	this.forkNewPlayer();
	            	// reset the counter value
	            	this.stepSinceFork = 0;
	            }
	            // if current node has a goal
	            if (maze.hasGoal(current)) {
	            	// tell that we have found a solution
	            	this.goalFound.set(true);
	                // move player to goal
	                maze.move(player, current);
	                // we initialize a new list then create a copy of the path from start value
	                List<Integer> resultingSubPath = new ArrayList<Integer>();
	                resultingSubPath.addAll(this.pathFromStart);
	        		// we then add the goal find sub path to the result
	                resultingSubPath.addAll(pathFromTo(start, current));
	                // search finished: return the sub path
	                return resultingSubPath;
	            }
	            // we add the current to the visited set
	            if (visited.add(current)) {
	            	// move player to current node
	                maze.move(player, current);
	                // increment the step counter value
	                this.stepSinceFork++;
	                // for every node nb adjacent to current
	                for (int nb: maze.neighbors(current)) {
	                	// if nb has not been already visited
	                	if (!visited.contains(nb)) {
                			// add nb to the nodes to be processed
	                        frontier.push(nb);
	                        // nb can be reached from current (i.e., current is nb's predecessor)
	                        predecessor.put(nb, current);
	                	}
	                }
	            }
	        }
	        // all nodes explored ore goal can have been found so we wait for the children
	        for (ForkJoinSolver child: this.children) {
	        	// Get the result of the current child
	        	List<Integer> searchResult = child.join();
	        	// If the child have found the goal or one of his children
	        	if (searchResult != null) {
	        		// we make a copy of the path from start of the list to store the concatenation result
	        		List<Integer> resultingPath = new ArrayList<Integer>();
	        		resultingPath.addAll(this.pathFromStart);
	        		// we then add the goal find sub path to the result
	        		resultingPath.addAll(searchResult);
	        		// we return his search result
	        		return resultingPath;
	        	}
	        }
		}
        // No child have found the goal
        return null;
    }
    
    /**
     * Instantiate a new ForkJoinSolver on each frontier node
     * remaining for the current parent and then
     * fork them as children
     */
    private void forkNewPlayer() {
    	// first we create a ForkJoinSolver instance on each node of the frontier stack
    	while (!this.frontier.empty()) {
    		// we start by getting the first node on the frontier stact
    		int currentFrontierNode = this.frontier.pop();
    		if (!this.visited.contains(currentFrontierNode)) {
	            // we compute the path to the start to the current node in case the created instance find the goal
	    		List<Integer> pathFromStart = pathFromTo(this.start, currentFrontierNode);
	    		// we remove the last one because it will be inside the next fork result
	    		pathFromStart.remove(pathFromStart.size()-1);
	    		// we then create a new instance of the ForkJoinSolver
	    		ForkJoinSolver fjsInst = new ForkJoinSolver(this.maze, this.forkAfter, this.visited, currentFrontierNode, pathFromStart, this.goalFound);
	    		// we add the new instance to the children list of the parent so it's easier to join them all at the end
	    		this.children.add(fjsInst);
	    		// then we fork the new instance
	    		fjsInst.fork();
    		}
    	}
    }
}