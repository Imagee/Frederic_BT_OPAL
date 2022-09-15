public class SimpleSqlExample01 {
    public void main(){

        String insert01 = "INSERT INTO tableA (id, name) VALUES (1 , tainted );" ;
        String insert02 = "INSERT INTO tableA (id, name) VALUES (2 , Freddy );" ;
        String insert03 = "INSERT INTO tableA (id, name) VALUES (3 , Mr.Nobody );" ;

        String randomString01 = "Dummy String 01 ";
        String randomString02 = "Random String 02";


        /*
        Nur "part1" wird erkannt , "insert04"s zweite hälfte wird nicht nicht erkannt.
        Problem "invokedynamic" ?
        String part1 ="INSERT INTO tableA (id, name)";
        String insert04 = part1 +" VALUES (2 , Freddy );";

           execInsert(insert04);
         */


        execInsert(insert01);
        execInsert(insert02);
        execInsert( "INSERT INTO tableA (id, name) VALUES (5 , Mr.SomeOne );");
        //execInsert(insert03);
        uninportantMethod(insert03);
        uninportantMethod(randomString01);

        String select = "SELECT * FROM tableA WHERE name = tainted";
        Object result = execSelect(select);
        Sink(result);

    }

    public void Sink(Object str){

    }

    public void execInsert(String stmt){

    }

    public Object execSelect(String stmt){
        return  stmt.split("");
    }

    public void uninportantMethod(String str){

    }
}
