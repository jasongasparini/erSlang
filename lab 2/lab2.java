import java.util.Scanner;
import java.util.ArrayList;
import java.util.Arrays;

class lab2 {
    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        
        int m = 0;
        int n = 0;

        System.out.print("Enter in a value for m: ");
        m = input.nextInt();

        System.out.print("Enter in a value for n: ");
        n = input.nextInt();
        input.close();

        
        ArrayList<ArrayList> list = new ArrayList<>();
        for(int count = m; count > 0; count--){
            ArrayList<Integer> subList = new ArrayList<>();
            for(int i = 0; i < n; i++){
                int value = count + (m*i);
                subList.add(value);
            }
            list.add(subList);
        }

        for (int i = 0; i < list.size(); i++) {
            System.out.println(list.get(i));
          }
    
    }
}