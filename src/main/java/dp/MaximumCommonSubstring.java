package dp;

/**
 * Find the longest common substring. Link {@link https://en.wikipedia.org/wiki/Longest_common_substring_problem}
 *
 * The longest common substring of the strings "ABABC", "BABCA" and "ABCBA" is string "ABC" of length 3.
 * Other common substrings are "A", "AB", "B", "BA", "BC" and "C".
 */
public class MaximumCommonSubstring {

	public static int maxCommonSubstring(String s1, String s2){
		//Memoization function
		int F[][] = new int[s2.length() + 1][s1.length() + 1];

		//Fill zero cases
		for(int i=0;i<s1.length()+1;i++){
			F[0][i] = 0;
		}

		for(int i=0;i<s2.length()+1;i++) {
			F[i][0] = 0;
		}

		int maxVal = 0;

		for(int i=1;i<s2.length()+1;i++){
			char s2c = s2.charAt(i-1);
			for(int j=1; j<s1.length()+1;j++){
				char s1c = s1.charAt(j-1);

				boolean charsEquals = s1c == s2c;

				F[i][j] =charsEquals ? F[i-1][j-1] + 1:0;

				maxVal = Math.max(F[i][j], maxVal);
			}
		}

		return maxVal;
	}

	public static void main(String[] args) {
		assert maxCommonSubstring("SUBSEQUENCE", "SUBEUENCS") == 4;
		assert maxCommonSubstring("aa", "ab") == 1;
		assert maxCommonSubstring("aakbbc", "aambbc") == 3;

	}
}
