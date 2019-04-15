package throttle;

import org.junit.Test;

import static org.junit.Assert.*;

public class DelayQueueThrottleTest {

	@Test
	public void testRequestsAllowed() throws InterruptedException {
		DelayQueueThrottle throttle = new DelayQueueThrottle(5);

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
		DelayQueueThrottle throttle = new DelayQueueThrottle(4);

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
		DelayQueueThrottle throttle = new DelayQueueThrottle(20);

		for(int i=0; i<20; i++){
			assertTrue(throttle.isAllowed(new Object()));
		}

		assertFalse(throttle.isAllowed(new Object()));
		Thread.sleep(1000);

		assertTrue(throttle.isAllowed(new Object()));
	}


}