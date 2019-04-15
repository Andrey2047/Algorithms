package throttle;

import java.util.concurrent.DelayQueue;
import java.util.concurrent.Delayed;
import java.util.concurrent.TimeUnit;


/**
 * Throttle implementation based on delay queue
 */
public class DelayQueueThrottle implements Throttle {

	private DelayQueue delayQueue = new DelayQueue();
	private int maxRequestsPerSecond;


	public DelayQueueThrottle(int maxRequestsPerSecond) {
		this.maxRequestsPerSecond = maxRequestsPerSecond;
	}

	@Override
	public synchronized boolean isAllowed(Object request) {
		long timestamp = System.currentTimeMillis();
		if(delayQueue.size() < maxRequestsPerSecond){
			delayQueue.put(new Entry(timestamp));
			return true;
		} else {
			Delayed poll = delayQueue.poll();
			if (poll == null){
				return false;
			} else {
				delayQueue.put(new Entry(timestamp));
				return true;
			}
		}
	}


	private class Entry implements Delayed {

		private static final int SECOND = 1000;
		private final long expiringTime;

		public Entry(long timestamp) {
			this.expiringTime = timestamp + SECOND;
		}

		@Override
		public long getDelay(TimeUnit timeUnit) {
			long diff = expiringTime - System.currentTimeMillis();
			return timeUnit.convert(diff, TimeUnit.MILLISECONDS);
		}

		@Override
		public int compareTo(Delayed delayed) {
			return (int) (delayed.getDelay(TimeUnit.MILLISECONDS) - delayed.getDelay(TimeUnit.MILLISECONDS));
		}
	}


}
