package throttle;

/**
 * Throttle implementation based on leaky bucket algorithm (https://en.wikipedia.org/wiki/Leaky_bucket).
 *
 * Smooth load. Do not allow to burst rate.
 */
public class LeakyBucketThrottle implements Throttle {

	public static final double SECOND = 1000.0;
	private long maxRate;
	private long lastChecked = System.currentTimeMillis();
	private double timeToGetToken;
	private long availableTokens;

	public LeakyBucketThrottle(long maxRate) {
		this.maxRate = maxRate;
		this.timeToGetToken = SECOND / maxRate;
		System.out.println("Tokens per time : " + timeToGetToken);

		this.availableTokens = 0;
	}

	@Override
	public boolean isAllowed(Object request) {
		long currentTime = System.currentTimeMillis();

		long elapsedTime = currentTime - lastChecked;
		System.out.println("Time diff - " + elapsedTime);


		int tokensAdded = (int) (Math.floor(elapsedTime / timeToGetToken));
		System.out.println("Tokens added: " + tokensAdded);

		if(tokensAdded > 0) {
			lastChecked = currentTime;
			availableTokens = Math.min(availableTokens + tokensAdded,  maxRate);
		}

		if(availableTokens > 0){
			availableTokens--;
			return true;
		} else{
			return false;
		}
	}
}
