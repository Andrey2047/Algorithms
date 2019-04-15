package throttle;

import java.util.LinkedList;
import java.util.Queue;

/**
 * Throttle implementation based on FIFO queue.
 */
public class SimpleQueueThrottle implements Throttle {

	private Queue<Long> timestampQueue = new LinkedList<>();

	private long lastCheckTime = System.currentTimeMillis();

	private long maxRate;

	public SimpleQueueThrottle(long maxRate) {
		this.maxRate = maxRate;
	}

	@Override
	public synchronized boolean isAllowed(Object request) {

		long currentTimestamp = System.currentTimeMillis();


		if(currentTimestamp - lastCheckTime > 1000) {
			timestampQueue.clear();
		}

		lastCheckTime = currentTimestamp;

		if(timestampQueue.size() < maxRate){
			timestampQueue.add(currentTimestamp);
			return true;
		} else {
			Long lastTimestamp = timestampQueue.peek();
			if(currentTimestamp - lastTimestamp > 1000) {
				timestampQueue.remove();
				timestampQueue.add(currentTimestamp);
				return true;
			} else {
				return false;
			}
		}
	}
}
