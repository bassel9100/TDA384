/**
Bassel Kanaan & Raphael Tournafond
**/

// Imports
import TSim.*;
import java.util.concurrent.Semaphore;

public class Lab1 {

	/*********** Arguments ***********/

	// The TSim interface used to interact with the trains
	private TSimInterface tsi;

	// Binary semaphores for each CS
	Semaphore s1 	= new Semaphore(1); 	// 1-7
	Semaphore s2	= new Semaphore(1); 	// 4-5-3-6
	Semaphore s3 	= new Semaphore(1); 	// 9
	Semaphore s4 	= new Semaphore(1); 	// 10-12
	Semaphore s5 	= new Semaphore(1); 	// 14
	Semaphore s6	= new Semaphore(1); 	// 15-17

	// Sensors position for easier code writing
	Sensor[] sp = {
		new Sensor(14,3),
		new Sensor(14,5),
		new Sensor(9,5),
		new Sensor(6,6),
		new Sensor(11,7),
		new Sensor(11,8),
		new Sensor(14,7),
		new Sensor(14,8),
		new Sensor(19,8),
		new Sensor(12,9),
		new Sensor(12,10),
		new Sensor(7,9),
		new Sensor(7,10),
		new Sensor(1,10),
		new Sensor(6,11),
		new Sensor(6,13),
		new Sensor(14,11),
		new Sensor(14,13)
	};

	/* Used to know the train direction */
	private enum Direction {
		forward, // the train is going from top station to down station
		backward, // reverse
		stopped // the train is stopped
	}

	/*********** Constructor ***********/

	public Lab1(int speed1, int speed2) {
		tsi = TSimInterface.getInstance(); // Get the tsim instance
		startTrafic(speed1, speed2); //Creates two threads for each of the trains
	}

	/************ Methods ***********/

	/* Used to start the two trains on the track */
	private void startTrafic(int speed1, int speed2) {

		// Creating the two trains task and initialize their start state (id, speed, direction)
		Train train1Task = new Train(1, speed1, Direction.forward);
		Train train2Task = new Train(2, speed2, Direction.backward);

		// Creating the two threads for each train
		Thread train1Thread = new Thread(train1Task);
		Thread train2Thread = new Thread(train2Task);

		// Lock the critical section corresponding to each train
		try {
			s1.acquire();
			s6.acquire();
		} catch (InterruptedException e) {
			e.printStackTrace();
			System.exit(1);
		}
		//Start the two threads
		train1Thread.start();
		train2Thread.start();
	}

	/*********** Classes ***********/

	/* Sensor class */

	private class Sensor {

		/* Arguments used to store the Sensor coordinates */
		private int x;
		private int y;

		public Sensor(int x, int y) {
			this.x = x;
			this.y = y;
		}

		/* Return true if the coordinates match */
		public boolean equals(int x, int y) {
			return this.x == x && this.y == y;
		}
	}

	/* Train class */

	private class Train implements Runnable {

		/* Arguments */

		private int id; // Train id
		private int speedBeforeStop; // Speed of the train before it stops (and then regain its same speed)
		private int speed; // Current speed of the train
		private Direction directionBeforeStop; // Direction of the train before it stops (and then regain its same direction)
		private Direction direction; // Current direction of the train

		/* Constructor */

		public Train(int id, int speed, Direction direction) {
			this.id = id;
			this.speedBeforeStop = speed;
			this.speed = speed;
			this.directionBeforeStop = direction;
			this.direction = direction;
		}

		/* Methods */

		/* Stop the train */
		private void stopNow() {
			// Save the train speed and direction
			this.speedBeforeStop = this.speed;
			this.directionBeforeStop = this.direction;
			setSpeed(0);
			// Update the direction
			this.direction = Direction.stopped;
		}

		/* Restart the train */
		private void goNow() {
			 // Set the train previous speed and direction
			setSpeed(this.speedBeforeStop);
			this.direction = this.directionBeforeStop;
		}

		/* Set the speed of the train using the tsim instance */
		private void setSpeed(int speed) {
			try {
				// Set the speed of the train using tsim
				tsi.setSpeed(id, speed);
				// Update the speed in the arguments of the train
				this.speed = speed;
			} catch (CommandException e) {
				e.printStackTrace();
				System.exit(1);
			}
		}

		/* Reverse the train direction (used at stations) */
		private void goBackward() {
			// If not stopped
			if (this.direction != Direction.stopped) {
				// Stop the train
				stopNow();
				try {
					// Wait until the train has come to a complete stop
					Thread.sleep(Math.abs(speed)*20+1000);
				} catch (InterruptedException e) {
					e.printStackTrace();
					System.exit(1);
				}
				// If the train direction was from up station to down station
				if (this.directionBeforeStop == Direction.forward) {
					// Go in the opposite direction
					this.direction = Direction.backward;
				} else {
					this.direction = Direction.forward;
				}
				// Regain its previous speed but in reverse
				setSpeed(-this.speedBeforeStop);
			}
		}

		/* Used to find the sensor number by comparing coordinates with a sensors list */
		private int findID(Sensor[] array, int x, int y) {
			// Loop through the array of sensors
			for (int i = 0; i < array.length; i++) {
				// If the current sensor have the searched coordinates
				if (array[i].equals(x, y)) {
					// We return its number (we add one because array indexes start to 0)
					return 1+i;
				}
			}
			// If there is no correspondence
			return 0;
		}

		/* Stop and wait for a semaphore to be released
		 * if the semaphore is already released the train stops and moves on directly */
		private void waitFor(Semaphore s) {
			/* if the critical section is already locked */
			if (!s.tryAcquire()) {
				// Stop the train
				stopNow();
				try {
					// Try to acquire the semaphore
					s.acquire();
				} catch (InterruptedException e) {
					e.printStackTrace();
					System.exit(1);
				}
				// Once the semaphore is acquired the train can go again
				goNow();
			}
		}

		/* Same as waitFor(Semaphore s) but change the selected switch direction */
		private void waitFor(Semaphore s, int x, int y, int d) {
			// Wait for the critical section
			waitFor(s);
			// Set the new position for the switch
			setSwitch(x, y, d);
		}

		/* Set a new switch position */
		private void setSwitch(int x, int y, int d) {
			try {
				// Set the specified switch to is new position
				tsi.setSwitch(x, y, d);
			} catch (CommandException e) {
				e.printStackTrace();
				System.exit(1);
			}
		}

		/* Release a semaphore and make sure that the semaphore is not already released */
		private void releaseIfPossible(Semaphore s) {
			if (s.availablePermits() == 0) {
				s.release();
			}
		}


		/* Run method */
		@Override
		public void run() {
			// Used to store the last sensor event
			SensorEvent se;
			// Used to store the current sensor ID corresponding to the last event
			int snb;
			// Start the train with its initial speed
			setSpeed(speed);
			// Start the main loop
			while(true) {
				try {
				// Wait until a new Sensor event
				se = tsi.getSensor(id);
				// If the sensor is activated
				if (se.getStatus() == SensorEvent.ACTIVE) {
					// We search the corresponding sensor number by using the sensor event coordinates
					snb = findID(sp, se.getXpos(), se.getYpos());
					// If the train is going forward (up to down)
					if (this.direction == Direction.forward) {
						switch (snb) {
							// Nothing to do
							case 1:
								break;
							case 2:
								break;
							case 3:
								waitFor(s2);
								break;
							case 4:
								waitFor(s2);
								break;
							case 5:
								releaseIfPossible(s2);
								break;
							case 6:
								releaseIfPossible(s2);
								break;
							case 7:
								waitFor(s3, 17, 7, TSimInterface.SWITCH_RIGHT);
								releaseIfPossible(s1);
								break;
							case 8:
								waitFor(s3, 17, 7, TSimInterface.SWITCH_LEFT);
								break;
							case 9:
								stopNow();
								if (s4.tryAcquire()) {
									tsi.setSwitch(15, 9, TSimInterface.SWITCH_RIGHT);
								} else {
									tsi.setSwitch(15, 9, TSimInterface.SWITCH_LEFT);
								}
								goNow();
								break;
							case 10:
								releaseIfPossible(s3);
								break;
							case 11:
								releaseIfPossible(s3);
								break;
							case 12:
								waitFor(s5, 4, 9, TSimInterface.SWITCH_LEFT);
								releaseIfPossible(s4);
								break;
							case 13:
								waitFor(s5, 4, 9, TSimInterface.SWITCH_RIGHT);
								break;
							case 14:
								stopNow();
								if (s6.tryAcquire()) {
									tsi.setSwitch(3, 11, TSimInterface.SWITCH_LEFT);
								} else {
									tsi.setSwitch(3, 11, TSimInterface.SWITCH_RIGHT);
								}
								goNow();
								break;
							case 15:
								releaseIfPossible(s5);
								break;
							case 16:
								releaseIfPossible(s5);
								break;
							case 17:
								goBackward();
								break;
							case 18:
								goBackward();
								break;
							default:
								System.err.println("Unknow sensor nb : " + snb);
								break;
						}
					} else
						// If the train is going backward
						if (this.direction == Direction.backward) {
							switch (snb) {
								case 1:
									goBackward();
									break;
								case 2:
									goBackward();
									break;
								case 3:
									releaseIfPossible(s2);
									break;
								case 4:
									releaseIfPossible(s2);
									break;
								case 5:
									waitFor(s2);
									break;
								case 6:
									waitFor(s2);
									break;
								case 7:
									releaseIfPossible(s3);
									break;
								case 8:
									releaseIfPossible(s3);
									break;
								case 9:
									stopNow();
									if (s1.tryAcquire()) {
										tsi.setSwitch(17, 7, TSimInterface.SWITCH_RIGHT);
									} else {
										tsi.setSwitch(17, 7, TSimInterface.SWITCH_LEFT);
									}
									goNow();
									break;
								case 10:
									waitFor(s3, 15, 9, TSimInterface.SWITCH_RIGHT);
									releaseIfPossible(s4);
									break;
								case 11:
									waitFor(s3, 15, 9, TSimInterface.SWITCH_LEFT);
									break;
								case 12:
									releaseIfPossible(s5);
									break;
								case 13:
									releaseIfPossible(s5);
									break;
								case 14:
									stopNow();
									if (s4.tryAcquire()) {
										tsi.setSwitch(4, 9, TSimInterface.SWITCH_LEFT);
									} else {
										tsi.setSwitch(4, 9, TSimInterface.SWITCH_RIGHT);
									}
									goNow();
									break;
								case 15:
									waitFor(s5, 3, 11, TSimInterface.SWITCH_LEFT);
									releaseIfPossible(s6);
									break;
								case 16:
									waitFor(s5, 3, 11, TSimInterface.SWITCH_RIGHT);
									break;
								case 17:
									break;
								case 18:
									break;
								default:
									System.err.println("Unknow sensor nb : " + snb);
									break;
							}
						}
					}
				} catch (CommandException | InterruptedException e) {
					e.printStackTrace();
					System.exit(1);
				}
			}
		}
	}
}
