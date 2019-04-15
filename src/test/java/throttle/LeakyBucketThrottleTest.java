package throttle;

import org.junit.Test;

import static org.junit.Assert.*;

public class LeakyBucketThrottleTest {
	@Test
	public void testRequestsAllowed() throws InterruptedException {
		LeakyBucketThrottle throttle = new LeakyBucketThrottle(5);

		assertTrue(throttle.isAllowed(new Object()));
		Thread.sleep(200);

		assertTrue(throttle.isAllowed(new Object()));
		Thread.sleep(200);

		assertTrue(throttle.isAllowed(new Object()));
		Thread.sleep(200);

		assertTrue(throttle.isAllowed(new Object()));
	}

	@Test
	public void testRequestsNotAllowed() throws InterruptedException {
		LeakyBucketThrottle throttle = new LeakyBucketThrottle(4);

		assertTrue(throttle.isAllowed(new Object()));
		Thread.sleep(100);

		assertTrue(throttle.isAllowed(new Object()));
		Thread.sleep(100);

		assertTrue(throttle.isAllowed(new Object()));
		Thread.sleep(100);

		assertTrue(throttle.isAllowed(new Object()));
		Thread.sleep(100);

		assertTrue(throttle.isAllowed(new Object()));
		Thread.sleep(100);

		assertFalse(throttle.isAllowed(new Object()));
		Thread.sleep(650);

		assertTrue(throttle.isAllowed(new Object()));
	}

	@Test
	public void testNotRegularLoad() throws InterruptedException {
		LeakyBucketThrottle throttle = new LeakyBucketThrottle(20);

		for(int i=0; i<21; i++){
			assertTrue(throttle.isAllowed(new Object()));
		}

		assertFalse(throttle.isAllowed(new Object()));
		Thread.sleep(650);

		assertTrue(throttle.isAllowed(new Object()));
	}
}