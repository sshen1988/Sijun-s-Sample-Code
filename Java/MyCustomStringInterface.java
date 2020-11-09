package edu.gatech.seclass;

/**
 * Interface created for use in Georgia Tech CS6300.
 *
 * This is an interface for a simple class that represents a string, defined
 * as a sequence of characters.
 *
 * This interface should NOT be altered in any way.
 */
public interface MyCustomStringInterface {

    /**
     * Returns the current string.
     * If the string is null, or has not been set to a value with setString, it should return null.
     *
     * @return Current string
     */
    String getString();

    /**
     * Sets the value of the current string
     *
     * @param string The value to be set
     */
    void setString(String string);

    /**
     * Returns the number of numbers in the current string, where a number is defined as a
     * continuous sequence of digits.
     *
     * If the current string is null, empty, or has not been set to a value, the method should return 0.
     *
     * Examples:
     * - countNumbers would return 2 for string "My numbers are 11 and 96".
     *
     * @return Number of numbers in the current string
     */
    int countNumbers();

    /**
     * Returns a string equivalent to the original string with n added to every digit in the string.
     * The digit will wrap from 8, 9.. to 0, 1, 2... whenever it is increased past 9.
     *
     * If 'reverse' is true, the order of the digits within every continuous sequence of digits will also be reversed.
     * If 'reverse' is false, the digits will remain in their original order within the string.
     *
     * Examples:
     * - For n=2 and reverse=false, "hello 90, bye 2" would be converted to "hello 12, bye 4".
     * - For n=4 and reverse=false, "1234567890" would be converted to "5678901234".
     * - For n=2 and reverse=true, "hello 90, bye 2" would be converted to "hello 21, bye 4".
     * - For n=4 and reverse=true, "1234567890" would be converted to "4321098765".
     *
     * @param n amount to increment or decrement every digit
     * @param reverse Boolean that indicates whether continuous sequences of digits should be reversed
     * @return String with the original string's digits incremented by n, in reverse order if indicated
     * @throws NullPointerException     If the current string is null
     * @throws IllegalArgumentException If n > 9 or n <=0 (and the current string is not null)
     */
    String addDigits(int n, boolean reverse);

    /**
     * Replace the individual digits in the current string, between startPosition and endPosition
     * (included), with the corresponding English names of those digits, with no letters
     * capitalized. The first character in the string is considered to be in Position 1.
     * Contiguous digits will be surrounded by asterisks and each digit will be converted individually.
     *
     * Examples:
     * - String 460 would be converted to *foursixzero*
     * - String 416 would be converted to *fouronesix*
     *
     * @param startPosition Position of the first character to consider
     * @param endPosition   Position of the last character to consider

     * @throws NullPointerException     If the current string is null
     * @throws IllegalArgumentException    If "startPosition" < 1 or "startPosition" > "endPosition" and the string is not null
     * @throws MyIndexOutOfBoundsException If "endPosition" is out of bounds (greater than the length of the string)
     * and 1 <= "startPosition" <= "endPosition" and the string is not null
     */
    void convertDigitsToNamesInSubstring(int startPosition, int endPosition);
}
