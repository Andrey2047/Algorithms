package throttle;

import org.junit.Test;

import static org.junit.Assert.*;

public class SimpleQueueThrottleTest {

	@Test
	public void testRequestsAllowed() throws InterruptedException {
		SimpleQueueThrottle throttle = new SimpleQueueThrottle(5);

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
		SimpleQueueThrottle throttle = new SimpleQueueThrottle(4);

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
		SimpleQueueThrottle throttle = new SimpleQueueThrottle(20);

		for(int i=0; i<20; i++){
			assertTrue(throttle.isAllowed(new Object()));
		}

		assertFalse(throttle.isAllowed(new Object()));
		Thread.sleep(1040);

		assertTrue(throttle.isAllowed(new Object()));
	}

}