package edu.gatech.seclass;

public class MyCustomString implements MyCustomStringInterface {
    private String myString = "";

    // Returns the current string;
    public String getString() {
        return this.myString;
    }


    @Override
    public void setString(String string) {
        this.myString = string;
    }


    @Override
    public int countNumbers() {
        int count = 0;
        boolean isANumber = false;
        for (int i = 0; i < myString.length(); i++) {
            if (myString.charAt(i) >= '0' && myString.charAt(i) <= '9' && !isANumber) {
                ++count;
            }
            isANumber = (myString.charAt(i) >= '0' && myString.charAt(i) <= '9');
        }
        return count;
    }


    @Override
    public String addDigits(int n, boolean reverse) throws NullPointerException, IllegalArgumentException{
        if (myString.length() == 0) throw new NullPointerException();
        if (n > 9 || n < 0) throw new IllegalArgumentException();
        StringBuilder res = new StringBuilder();
        int i = 0;
        int len = myString.length();

        while (i < len) {
            StringBuilder temp = new StringBuilder();

            while (i < len && myString.charAt(i) >= '0' && myString.charAt(i) <= '9') {
                int k = (Character.getNumericValue(myString.charAt(i)) + n) % 10;
                if (reverse) {
                    temp.insert(0, k);
                } else {
                    temp.append(k);
                }
                i++;
            }

            res.append(temp);

            if (i < len) res.append(myString.charAt(i++));

        }
        return res.toString();
    }


    @Override
    public void convertDigitsToNamesInSubstring(int startPosition, int endPosition) throws NullPointerException, IllegalArgumentException, MyIndexOutOfBoundsException {
        if (myString.length() == 0) throw new NullPointerException();
        if (startPosition < 1 || startPosition > endPosition) throw new IllegalArgumentException();
        if (endPosition > myString.length()) throw new MyIndexOutOfBoundsException();

        StringBuilder myStringBuilder = new StringBuilder();
        int i = startPosition - 1;

        while (i < endPosition){
            StringBuilder temp = new StringBuilder();

            while (i < endPosition && myString.charAt(i) >= '0' && myString.charAt(i) <= '9'){
                String st = intToString(Character.getNumericValue(myString.charAt(i)));
                temp.append(st);
                i++;
            }

            if (temp.length() != 0){
                temp.insert(0, '*');
                temp.append('*');
                myStringBuilder.append(temp.toString());
            }

            if (i < endPosition) myStringBuilder.append(myString.charAt(i++));

        }


        StringBuilder resStringBuilder = new StringBuilder();
        for (int k = 0; k < startPosition - 1; k++){
            resStringBuilder.append(myString.charAt(k));
        }

        resStringBuilder.append(myStringBuilder);

        for (int k = endPosition; k < myString.length(); k++){
            resStringBuilder.append(myString.charAt(k));
        }

        this.myString = resStringBuilder.toString();

    }

    private static String intToString(int n) {
        String[] numNames = {"zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"};
        return numNames[n];
    }

    public static void main(String[] args){
        MyCustomString mcs = new MyCustomString();
        mcs.setString("hello 90, bye 2");
        System.out.println(mcs.getString());
        System.out.println("Count numbers: " + mcs.countNumbers());
        System.out.println("Add digits: " + mcs.addDigits(2, true));
        mcs.convertDigitsToNamesInSubstring(5,10);
        System.out.println("Convert Digits to Names: " + mcs.getString());
    }

}