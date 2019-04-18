package dp;

public class AbbreviationTask {

	static boolean isLetterUpperCase(char c) {
		return c < 97;
	}

	static char toUpperCase(char c) {
		return (char) (c - 32);
	}

	static String abbreviation(String a, String b) {

		boolean[][] F = new boolean[a.length() + 1][b.length() + 1];


		// initializing the first raw to all false; ie. if b is
		// not empty, isValid will always be false
		F[0][0] = true;

		//fill first column for empty strings
		for(int i=1; i <= a.length(); i++){
			char currentChar = a.charAt(i-1);
			if(!isLetterUpperCase(currentChar)){
				F[i][0] = F[i-1][0];
			} else {
				F[i][0] = false;
			}
		}


		//Fill Dp table:
		for (int j=1; j<b.length()+1;j++) {

			char currentBChar = b.charAt(j-1);

			for (int i=1; i<a.length()+1;i++) {
				char currentAChar = a.charAt(i-1);
				if(!isLetterUpperCase(currentAChar)){
					// elif uppercase a == b, set = prev character bool. or just eat a.
					if (toUpperCase(currentAChar) == currentBChar){
						F[i][j] = F[i-1][j-1] || F[i-1][j];
					} else {
						//else just eat a
						F[i][j] = F[i-1][j];
					}
				} else {
					// when the characters are equal and a in upper case then set = previous character bool.
					if(currentAChar == currentBChar){
						F[i][j] = F[i-1][j-1];
					} else {
						F[i][j] = false;
					}
				}
			}
		}

		return F[a.length()][b.length()] ? "YES" : "NO";

	}

	public static void main(String[] args) {
		String a = "daBcd";
		String b = "ABC";

		System.out.println(abbreviation(a, b));


	}


}
