package dp;

import org.junit.Assert;

/**
 * https://neerc.ifmo.ru/wiki/index.php?title=%D0%97%D0%B0%D0%B4%D0%B0%D1%87%D0%B0_%D0%BE_%D1%80%D1%8E%D0%BA%D0%B7%D0%B0%D0%BA%D0%B5
 */
public class KnapsackProblem {

	/**
	 * Knapsack problem when we have only one think allowed.
	 */
	public static int calculateMaxValue1_0(int sackVolume, int[] thingVolumes, int[]thingValues){
		int F[][] = new int[thingValues.length+1][sackVolume + 1];

		//Fill with start values:
		for(int i=0;i<thingValues.length+1;i++){
			F[i][0] = 0;
		}

		for(int w=0;w<sackVolume+1;w++){
			F[0][w] = 0;
		}

		//For calculating max value will use recurent formula:
		//F[i, w] = max{V[i-1, w], V[i-1, w - thingVolumes[i]] + thinkValues[i]}

		int maxValue = 0;

		for(int i=1;i<thingVolumes.length+1;i++){
			for(int w=1; w<sackVolume+1; w++){

				if(w - thingVolumes[i-1] >= 0){
					F[i][w] = Math.max(F[i-1][w], F[i-1][w-thingVolumes[i-1]] + thingValues[i-1]);
				} else {
					F[i][w] = F[i-1][w];
				}

				maxValue = Math.max(maxValue, F[i][w]);
			}
		}

		return maxValue;

	}

	/**
	 * Knapsack problem when we have unlimited count of each think.
	 */
	public static int calculateMaxValue(int sackVolume, int[] thingVolumes, int[]thingValues){

		int[]F = new int[sackVolume + 1];

		F[0] = 0;

		//Using reccurent formula:
		//F[i] = max(thingValues[j] + F[i - thingVolumes[j]])

		for (int i=1;i<sackVolume+1; i++){
			for(int j=0;j<thingVolumes.length;j++){
				int prevValue = i - thingVolumes[j];
				if(prevValue >= 0) {
					F[i] = Math.max(F[i], F[prevValue] + thingValues[j]);
				}
			}
		}

		return F[sackVolume];

	}

	public static void main(String[] args) {
		Assert.assertEquals(8, calculateMaxValue1_0(8, new int[]{2,3,4,5}, new int[]{1,2,5,6}));
		Assert.assertEquals(8, calculateMaxValue(10, new int[]{2,3,4,5}, new int[]{1,2,5,6}));
	}
}
