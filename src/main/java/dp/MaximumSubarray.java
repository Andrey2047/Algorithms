package dp;

/**
 * Link https://en.wikipedia.org/wiki/Maximum_subarray_problem
 */
public class MaximumSubarray {

	static int[] maxSubarray(int[] arr) {

		int max_ending_here = arr[0];
		int max_so_far = arr[0];
		int maxSum = arr[0];

		for(int i=1; i<arr.length; i++){
			max_ending_here = Math.max(max_ending_here + arr[i], arr[i]);
			max_so_far = Math.max(max_so_far, max_ending_here);
			maxSum = Math.max(Math.max(maxSum+arr[i], maxSum), arr[i]);
		}

		return new int[]{max_so_far, maxSum};
	}

	public static void main(String[] args) {
		int[] result = maxSubarray(new int[]{-1, 2, 3, -4, 5, 10});
		System.out.println(result[0] + " "+result[1]);
	}
}
