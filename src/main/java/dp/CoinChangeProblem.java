package dp;

import java.util.Arrays;

public class CoinChangeProblem {

	static long getWays(long n, long[] c) {

		Arrays.sort(c);

		//Will use reccurent formula:
		//if w >= c[i] T[i, w] = T[i-1, w] + T[i, w-c[i]]
		// if w< c[i] T[i, w] = T[i-1, w]


		//Fill for w = 0 c={0}

		int[][] T = new int[c.length][(int)(n+1)];

		for(int i=0;i<n+1;i++){
			T[0][i] = 1;
		}

		for(int i=1; i<c.length; i++) {
			long currentCoin = c[i];
			for (int j = 1; j < n+1; j++) {
				if(j>= currentCoin) {
					T[i][j] = T[i-1][j] + T[i][(int)(j-currentCoin)];
				} else {
					T[i][j] = T[i-1][j];
				}
			}
		}

		for(int i=0; i<c.length; i++) {
			for (int j = 0; j < n+1; j++) {
				System.out.print(T[i][j] + " ");
			}
			System.out.println();
		}

		return T[c.length-1][(int)(n)];

	}


	public static void main(String[] args) {
		System.out.println(getWays(4, new long[]{1,2,3}));
	}

}
