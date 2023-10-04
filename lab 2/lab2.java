import java.util.Scanner;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.InputMismatchException;

class lab2 {
    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        
        int X = 0;
        int Y = 0;

        // Get input for X
        while (true) {
            try {
                System.out.print("Enter a value for X: ");
                X = input.nextInt();
                if (X >= 0) {
                    break; // Break out of the loop if input is a valid non-negative integer
                } else {
                    System.out.println("Invalid input. Please enter a non-negative integer.");
                }
            } catch (InputMismatchException e) {
                System.out.println("Invalid input. Please enter an integer.");
                input.nextLine(); // Consume the invalid input
            }
        }

        // Get input for Y
        while (true) {
            try {
                System.out.print("Enter a value for Y: ");
                Y = input.nextInt();
                if (Y >= 0) {
                    break; // Break out of the loop if input is a valid non-negative integer
                } else {
                    System.out.println("Invalid input. Please enter a non-negative integer.");
                }
            } catch (InputMismatchException e) {
                System.out.println("Invalid input. Please enter an integer.");
                input.nextLine(); // Consume the invalid input
            }
        }


        input.close();

        
        ArrayList<ArrayList> list = new ArrayList<>();
        for(int count = X; count > 0; count--){
            ArrayList<Integer> subList = new ArrayList<>();
            for(int i = 0; i < Y; i++){
                int value = count + (X*i);
                subList.add(value);
            }
            list.add(subList);
        }

        for (int i = 0; i < list.size(); i++) {
            System.out.println(list.get(i));
          }
    
    }
}