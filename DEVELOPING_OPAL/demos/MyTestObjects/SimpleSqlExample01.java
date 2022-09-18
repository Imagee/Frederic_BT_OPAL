public class SimpleSqlExample01 {
    String field01 = "base";
    String field02 = "home02";
    public void main(boolean a){

        String insert01 = "INSERT INTO tableA (id, name) VALUES (1 , tainted );" ;
        String insert02 = "INSERT INTO tableA (id, name) VALUES (2 , Freddy );" ;
        String insert03 = "INSERT INTO tableA (id, name) VALUES (3 , Mr.Nobody );" ;

        String randomString01 = "Dummy String 01 ";
        String randomString02 = "Random String 02";

        field01 = "INSERT INTO tableA (id, name) VALUES (7 , Mr.field );" ;

        field02 = "Nothing init";


        /*
        Nur "part1" wird erkannt , "insert04"s zweite hälfte wird nicht nicht erkannt.
        Problem "invokedynamic" ?
        String part1 ="INSERT INTO tableA (id, name)";
        String insert04 = part1 +" VALUES (2 , Freddy );";

           execInsert(insert04);
         */

        if(a){
            insert02 = "war true";
            field01 = "INSERT INTO tableA (id, name) VALUES (9 , Mr.field_true );" ;
        }else {
            insert02 = "was false";
            field01  = "INSERT INTO tableA (id, name) VALUES (10 , Mr.field_false );" ;
        }



        execInsert(insert01);
        execInsert(insert02);
        execInsert( "INSERT INTO tableA (id, name) VALUES (5 , Mr.SomeOne );");
        execInsert(field01);
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
