package dp;

import java.util.LinkedList;

public class LongestCommonSubsequence {

	/**
	 * Find longest common subsequence and return it
	 * @param a
	 * @param b
	 * @return
	 */
	static int[] longestCommonSubsequence(int[] a, int[] b) {

		int F[][] = new int[a.length+1][b.length+1];

		for(int i=0;i<a.length+1;i++){
			F[i][0] = 0;
		}

		for(int i=0;i<b.length+1;i++){
			F[0][i] = 0;
		}

		//Will use reccurent formula:
		//			if a[i] == b[j]: F[i-1][j-1] + 1
		//F[i][j] =
		//			else max(F[i-1][j], F[i][j-1])
		for(int i=1;i<a.length+1;i++){
			for(int j=1;j<b.length+1;j++){
				if(a[i-1] == b[j-1]) {
					F[i][j] = F[i-1][j-1] + 1;
				} else {
					F[i][j] = Math.max(F[i-1][j], F[i][j-1]);
				}
			}
		}

		for(int i=0;i<a.length+1;i++){
			for(int j=0;j<b.length+1;j++){
				System.out.print(F[i][j]+" ");
			}
			System.out.println();
		}

		int il = a.length;
		int jl = b.length;

		LinkedList<Integer> result = new LinkedList<>();

		//Use backtracking alg to find subsequence.
		while(F[il][jl] > 0) {
			if(F[il][jl] > F[il-1][jl] && F[il][jl] > F[il][jl-1]){
				il = il-1;
				jl = jl-1;
				result.add(a[il]);
			} else if(F[il][jl] == F[il][jl-1]){
				jl = jl-1;
			} else {
				il = il-1;
			}
		}

		System.out.println(F[a.length][b.length]);

		int size = result.size();
		int[]r = new int[size];
		for(int i = 0; i< size; i++){
			r[i] = result.removeLast();
		}

		return r;
	}

	public static void main(String[] args) {
		int[] o = longestCommonSubsequence(new int[]{1, 2, 3, 4, 1}, new int[]{3, 4, 1, 2, 1, 3});
		System.out.println(o);
	}
}
