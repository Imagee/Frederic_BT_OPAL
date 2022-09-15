public class StaticTaintExample02 {

    public void main(){

        String x = Source("tainted");
        String y = "Bob";
        String z = x;
        String w = y + z;
        Sink(w);
    }

    public String Source(String str){
        return str;
    }

    public void Sink(String str){

    }


}
