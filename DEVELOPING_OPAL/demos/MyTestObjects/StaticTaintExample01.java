public class StaticTaintExample01 {

    public void main(){
        String x = Source("taint");
        String y = "Bob";
        String z = x;
        Sink(z);
    }

    public String Source(String str){
        return str+"ed";
    }

    public void Sink(String str){

    }


}
