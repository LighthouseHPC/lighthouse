import java.io.*;
import java.util.*;

/* 
  Some java functions I wrote for my own convenience.
*/

public class filter{
  public static void main(String[] args){
    String path = "/home/javed/Desktop/matrix_properties/matrices/";
     System.out.println("#");
     System.out.println("# Script");
     System.out.println("#");
     System.out.println("clear");
    //deleteGarbage(path);
    //generateParallelCommands(path);
    //generateConversionCommands(path);
    //processParallelData(path);
    generatePropertyComputationCommands(path);
    //addLink(path);
     System.out.println("exit 0");
  }

  static void processParallelData(String path){
    File dir = new File(path);
    File[] files = dir.listFiles();
    int counter = 0;
    for(int i = 0; i < files.length; i++){
      if(files[i].getName().contains(".mtx")){
        counter++;
      }
    }
    String[] mats = new String[counter];
    counter = 0;
    for(int i = 0; i < files.length; i++){
      if(files[i].getName().contains(".mtx")){
        mats[counter] = files[i].getName().replace(".mtx","");
        counter++;
      }
    }
    BufferedReader br = null;
    PrintWriter out = null;
    StringBuilder sb = null;

    String s = "";
    String l = "";
    try{
      for(int i = 0; i < mats.length; i++){      
        out = new PrintWriter(new BufferedWriter(new FileWriter(path+"fixed/"+mats[i]+".txt")));
        sb = new StringBuilder("");
        for(int j = 0; j < files.length; j++){
          s = files[j].getName(); 
          if(s.contains(mats[i]) && s.contains(".txt")){
            br = new BufferedReader(new FileReader(path+s));
            l = br.readLine()+"\n";
            sb.append(l);
            br.close();
          }
        }
        out.write(sb.toString());
        sb = null;
        out.close();
        System.out.println(mats[i]+".txt");
      }
    }catch(Exception x){
      x.printStackTrace();
    } 
  }

  static void addLink(String path){
    try {
      BufferedReader br_mtx = null;
      BufferedReader br_txt = null;
      StringBuilder sb = null;
      String sCurrentLine;
      String link = "";
      String text_file;      
      File dir = new File(path);
      int i = 1;
      int c;
      for (File child : dir.listFiles()) {
        if(child.getName().contains(".mtx")){
          br_mtx = new BufferedReader(new FileReader(path+child.getName()));
          sCurrentLine = br_mtx.readLine();
          c = 0;
          while(sCurrentLine.contains("%")){
            c++;            
            sCurrentLine = br_mtx.readLine();
            if(c == 3){
              link = sCurrentLine;
            }
          }
          if (br_mtx != null)br_mtx.close();
          text_file =  path+(child.getName().replace(".mtx",".txt"));

          br_txt = new BufferedReader(new FileReader(text_file));
          sb = new StringBuilder(link+"\n");
          sCurrentLine = br_txt.readLine();
          while(sCurrentLine != null){
            if(!sCurrentLine.contains("%") && !sCurrentLine.equals(""))            
              sb.append(sCurrentLine+"\n");
            sCurrentLine = br_txt.readLine();
          }        
          if (br_txt != null)br_txt.close();
          PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter(text_file)));
          out.write(sb.toString(),0,(sb.toString()).length());
          sb = null;
          out.close();
          System.out.println(i+". Fixed: "+child.getName());
          i++;         
        }
      }
    }catch (Exception e) {
      e.printStackTrace();
    }
  }

  static void generateParallelCommands(String path){
    File dir = new File(path);
    File[] files = dir.listFiles();
    int counter = 1;
    String s = "";
    int process=4,k=0;
    
    Arrays.sort(files, new Comparator<File>(){
      public int compare(File f1, File f2){
        return Long.valueOf(f1.length()).compareTo(f2.length());
      }
    });
    for(int i = 0; i < files.length; i++){
      if(files[i].getName().contains(".petsc") && !files[i].getName().contains(".info")){
        s = path+files[i].getName();
        k = 0;
        print(counter,process,"bicg","asm",s,k++);
        print(counter,process,"bicg","bjacobi",s,k++);
        print(counter,process,"bicg","none",s,k++);

        print(counter,process,"bcgs","asm",s,k++);
        print(counter,process,"bcgs","sor",s,k++);
        print(counter,process,"bcgs","bjacobi",s,k++);
        print(counter,process,"bcgs","none",s,k++);

        print(counter,process,"cg","asm",s,k++);
        print(counter,process,"cg","sor",s,k++);
        print(counter,process,"cg","bjacobi",s,k++);
        print(counter,process,"cg","none",s,k++);

        print(counter,process,"cgs","asm",s,k++);
        print(counter,process,"cgs","sor",s,k++);
        print(counter,process,"cgs","bjacobi",s,k++);
        print(counter,process,"cgs","none",s,k++);

        print(counter,process,"gmres","asm",s,k++);
        print(counter,process,"gmres","sor",s,k++);
        print(counter,process,"gmres","bjacobi",s,k++);
        print(counter,process,"gmres","none",s,k++);

        print(counter,process,"fgmres","asm",s,k++);
        print(counter,process,"fgmres","sor",s,k++);
        print(counter,process,"fgmres","bjacobi",s,k++);
        print(counter,process,"fgmres","none",s,k++);

        print(counter,process,"tfqmr","asm",s,k++);
        print(counter,process,"tfqmr","sor",s,k++);
        print(counter,process,"tfqmr","bjacobi",s,k++);
        print(counter,process,"tfqmr","none",s,k++);

        counter++;
      }
    }
  }

  static void print(int counter, int n, String kspType, String pcType, String s, int k){
    System.out.println("echo \""+counter+"."+k+"\"");
    System.out.println("mpiexec -n 4 ./parallel -f "
      + s + " -ksp_type "+kspType+" -pc_type "+pcType+" > " + s.replace(".petsc", ("_"+k+".txt")));
  }

  static void generateConversionCommands(String path){
    File dir = new File(path);
    File[] files = dir.listFiles();
    
    Arrays.sort(files, new Comparator<File>(){
      public int compare(File f1, File f2){
        return Long.valueOf(f1.length()).compareTo(f2.length());
      }
    });
    for(int i = 0; i < files.length; i++){
      if(files[i].getName().contains(".mtx")){
        System.out.println("echo \""+i+"/"+files.length+"\"");
        System.out.println("./convert -fin "
          + path+files[i].getName() + " -fout " 
          + path+files[i].getName().replace("mtx", "petsc"));
      }
    }
  }

  static void generatePropertyComputationCommands(String path){
    File dir = new File(path);
    File[] files = dir.listFiles();
    
    Arrays.sort(files, new Comparator<File>(){
      public int compare(File f1, File f2){
        return Long.valueOf(f1.length()).compareTo(f2.length());
      }
    });
    for(int i = 0; i < files.length; i++){
      if(files[i].getName().contains(".mtx")){
        System.out.println("echo \""+i+"/"+files.length+"\"");
        System.out.println("./properties -f "
          + path+files[i].getName() + " > " 
        + path+files[i].getName().replace(".mtx","_props.txt"));
      }
    }
  }

  static void deleteGarbage(String path){
    try {
      BufferedReader br = null;
      String sCurrentLine;
      File dir = new File(path);
      int i = 1;
      for (File child : dir.listFiles()) {
        if(child.getName().contains(".mtx")){
          br = new BufferedReader(new FileReader(path+child.getName()));
          sCurrentLine = br.readLine();
          //System.out.println(sCurrentLine);
          if(sCurrentLine.contains("pattern") || sCurrentLine.contains("array")){
            if(child.delete()){
              System.out.println(child.getName() + " is deleted! Reason: Pattern or array matrix");
            }else{
              System.out.println("Delete operation is failed.");
            }
          }
          if (br != null)br.close();
        }         
      }
    }catch (Exception e) {
      e.printStackTrace();
    }

    try {
      BufferedReader br = null;
      String sCurrentLine;
      File dir = new File(path);
      int i = 1;
      for (File child : dir.listFiles()) {
        if(child.getName().contains(".mtx")){
          br = new BufferedReader(new FileReader(path+child.getName()));
          while(true){
            sCurrentLine = br.readLine();
            if(sCurrentLine.contains("%")){
              sCurrentLine = br.readLine();
            }
            else{
              break;
            }
          }
          String[] s = sCurrentLine.split(" ");
          if(!s[0].equals(s[1])){
            if(child.delete()){
              System.out.println(child.getName() + " is deleted! Reason: not square");
            }else{
              System.out.println("Delete operation is failed.");
            }
          }
          if (br != null)br.close();
        }         
      }
    }catch (Exception e) {
      e.printStackTrace();
    }
  }

  static void generateSequentialCommands(String path){
    File dir = new File(path);
    File[] files = dir.listFiles();
    
    Arrays.sort(files, new Comparator<File>(){
      public int compare(File f1, File f2){
        return Long.valueOf(f1.length()).compareTo(f2.length());
      }
    });
    for(int i = 0; i < files.length; i++){
      if(files[i].getName().contains(".mtx")){
            System.out.println("echo \""+i+"/"+files.length+"\"");
            System.out.println("./sequential -f "
              + path+files[i].getName() + " > " 
              + path+files[i].getName().replace("mtx", "txt"));
      }
    }
  }
}