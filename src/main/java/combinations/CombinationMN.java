package combinations;

/**
 * Given an array of size n, generate and print all possible combinations of r elements in array.
 * For example, if input array is {1, 2, 3, 4} and r is 2, then output should be {1, 2}, {1, 3}, {1, 4}, {2, 3}, {2, 4} and {3, 4}.
 *
 * Use recursive method described in https://www.geeksforgeeks.org/print-all-possible-combinations-of-r-elements-in-a-given-array-of-size-n/
 */

public class CombinationMN {

	public static void printAllCombinations(int k, int[] a) {
		printAllCombinationInner(k, a, new int[k], 0, a.length);
	}

	private static void printAllCombinationInner(int k, int[] originalList, int[] formedList, int start, int end) {
		if(k == 0) {
			printArray(formedList);
		} else {
			for(int i=start; i<end ; i++){
				formedList[k-1] = originalList[i];
				printAllCombinationInner(k-1, originalList, formedList, i+1, end);
			}
		}
	}
	private static void printArray(int a[]){
		for(int i=0; i<a.length;i++){
			System.out.print(a[i] + " ");
		}
		System.out.println();

	}

	public static void main(String[] args) {
		printAllCombinations(3, new int[]{1,2,3,4});
	}
}
