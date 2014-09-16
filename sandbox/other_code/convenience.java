import java.io.*;
import java.util.*;
import java.net.URL;
import java.net.URLConnection;

/* 
  Some java functions I wrote for processing text files and generating scripts. 
  There are much better ways of doing this. I haven't learned them yet.
  Hence, this file.
*/

public class convenience{
  public static void main(String[] args){
    String path = "/home/javed/Desktop/Florida_Sequential/Data/";
    //String path = "/home/javed/Desktop/matrix_properties/properties/";
    
    // System.out.println("#");
    // System.out.println("# Script");
    // System.out.println("#");
    // System.out.println("clear");
    try{
    // deleteGarbage(path);
    // generateParallelCommands(path);
    // generateConversionCommands(path);
    // processParallelData(path);
     generatePropertyComputationCommands(path);
    // addLink(path);
    // generateMatlabScript(path);
    // generateTargetClassVector(path);
    // dataset();
    // generateRandomTargetClassVector();
    // generateParallelTargetClassVector(path);
    // downloadProperties(path);
    }catch(Exception x){
      x.printStackTrace();
    }
    // System.out.println("exit 0");
  }

  static void generateTargetClassVector2(String path){
    try{
      File dir = new File(path);
      File[] files = dir.listFiles();
      PrintWriter out = new PrintWriter(new 
        BufferedWriter(new FileWriter("target_class.txt")));
      StringBuilder sb = new StringBuilder("Tc = [");

      int counter = 1;
      int ct = 0;
      Arrays.sort(files);
      for(int i = 0; i < files.length; i++){
        BufferedReader br = null;
        if(files[i].getName().contains(".txt")){
          if(i%2 ==0)
            System.out.println(counter+" "+files[i].getName());
          br = new BufferedReader(new FileReader(path+files[i].getName()));
          String[] s;
          String l = "";
          double c,t,r;
          String ksp="",pc="";
          double lowest = Double.MAX_VALUE;
          int cls = 42;
          l = br.readLine();
          while(l != null){
            if(l.contains("%")){
              l = br.readLine();
            }
            l = l.replace("Failed","-99");
            l = l.replace("|",";");
            s = l.split(";");
            c = Double.parseDouble(s[2]);
            if(c >= 0){
              t = Double.parseDouble(s[3]);
              if(t < lowest){
                lowest = t;
                ksp = s[0].trim();
                pc = s[1].trim();                
              }
            }
           l = br.readLine();
          }
          sb.append(getCls(ksp,pc)+" ");
          br.close();         
          counter++;
        }        
      }
      System.out.println(ct);      
      sb.append("];\n");
      out.write(sb.toString());
      out.close();
    }catch(Exception x){
      x.printStackTrace();
    }    
  }


  static void dataset(){
    try{
      String path = "/home/javed/Desktop/matrix_properties/properties/";
      File dir = new File(path);
      File[] files = dir.listFiles();
      int counter = 1;

      Arrays.sort(files);

      int minDim = 100000;
      String minDimMat = "";
      int maxDim = 0;
      String maxDimMat = "";
      int minNnz = 100000;
      String minNnzMat = "";
      int maxNnz = 0;
      String maxNnzMat = "";
      int nvSymCount = 0;
      int nvUnSymCount = 0;
      int stSymCount = 0;
      int stUnSymCount = 0;
      int diagDomCount = 0;
      int diagNonDomCount = 0;
      double maxRowVar = 0;
      String maxRowVarMat = "";
      double maxColVar = 0;
      String maxColVarMat = "";
      
      for(int i = 0; i < files.length; i++){
        BufferedReader br = null;
        if(files[i].getName().contains(".txt")){
          System.out.println(counter+" "+files[i].getName());
          br = new BufferedReader(new FileReader(path+files[i].getName()));
          String s[] = null;
          String l = "";
          double v;
          
          l = br.readLine();
          while(l != null){
            s = l.split(":");
            v = Double.parseDouble(s[1]);
            if(l.contains("Rows")){
              if(v < minDim) {minDim = (int)v; minDimMat = files[i].getName();}
              if(v > maxDim) {maxDim = (int)v; maxDimMat = files[i].getName();}
            }
            if(l.contains("Nonzeros")){
              if(v < minNnz) {minNnz = (int)v; minNnzMat = files[i].getName();}
              if(v > maxNnz) {maxNnz = (int)v; maxNnzMat = files[i].getName();}
            }
            if(l.contains("Numerical Symmetry")){
              if(v == 1) nvSymCount++;
              else nvUnSymCount++;
            }
            if(l.contains("Structural Symmetry")){
              if(v == 1) stSymCount++;
              else stUnSymCount++;
            }
            if(l.contains("Row Diagonal Dominance")){
              if(v >= 1) diagDomCount++;
              else diagNonDomCount++;
            }  
            if(l.contains("Row variance")){
              if(v > maxRowVar) {maxRowVar = v; maxRowVarMat = files[i].getName();}            
            }
            if(l.contains("Column variance")){
              if(v > maxColVar) {maxColVar = v; maxColVarMat = files[i].getName();}            
            } 
            l = br.readLine();
          }
          
          br.close();
        }        
      }
      System.out.println("Minimum dimension: "+minDim+" | Matrix: "+minDimMat);
      System.out.println("Maximum dimension: "+maxDim+" | Matrix: "+maxDimMat);
      System.out.println("Minimum Nnz: "+minNnz+" | Matrix: "+minNnzMat);
      System.out.println("Maximum Nnz: "+maxNnz+" | Matrix: "+maxNnzMat);
      System.out.println("Max. row variance: "+maxRowVar+" | Matrix: "+maxRowVarMat);
      System.out.println("Max. col variance: "+maxColVar+" | Matrix: "+maxColVarMat);
      System.out.println("Numerical symmetric matrices:"+nvSymCount);
      System.out.println("Numerical unsymmetric matrices:"+nvUnSymCount);
      System.out.println("Structurally symmetric matrices:"+stSymCount);
      System.out.println("Structurally unsymmetric matrices:"+stUnSymCount);
      System.out.println("Diagonally dominant matrices:"+diagDomCount);
      System.out.println("Diagonally nondominant matrices:"+diagNonDomCount);      
    }catch(Exception x){
      x.printStackTrace();
    }    
  }

  static void generateParallelTargetClassVector(String path){
    try{
      File dir = new File(path);
      File[] files = dir.listFiles();
      PrintWriter out = new PrintWriter(new 
        BufferedWriter(new FileWriter(path+"target_class_p.txt")));
      StringBuilder sb = new StringBuilder("Tc = [");

      int counter = 1;
      Arrays.sort(files);
      for(int i = 0; i < files.length; i++){
        BufferedReader br = null;
        if(files[i].getName().contains(".txt")){
          System.out.println(counter+" "+files[i].getName());
          br = new BufferedReader(new FileReader(path+files[i].getName()));
          String[] s;
          String l = "";
          double c,t,r;
          String ksp,pc;
          double lowest = Double.MAX_VALUE;
          int cls = 28;
          l = br.readLine();
          while(l != null){
            if(l.contains("%"))l = br.readLine();
            l = l.replace("Failed","-99");
            l = l.replace("|",";");
            s = l.split(";");
            if(s.length == 6){              
              c = Double.parseDouble(s[2]);
              if(c >= 0){
                t = Double.parseDouble(s[3]);
                if(t < lowest){
                  lowest = t;
                  ksp = s[0];
                  pc = s[1];
                  cls = getParallelCls(ksp.trim(),pc.trim());
                }
              }
            }
            l = br.readLine();
          }
          sb.append(cls+" ");
          br.close();
          counter++;
        }        
      }
      sb.append("];\n");
      out.write(sb.toString());
      out.close();
    }catch(Exception x){
      x.printStackTrace();
    }    
  }

  static int getParallelCls(String ksp, String pc){
    String m = ksp + " " + pc;
    int cls = 28;
    switch(m){
      case "cg asm": cls = 1; 
                        break;
      case "cg bjacobi": cls = 2; 
                        break;
      case "cg sor"   : cls = 3; 
                        break;
      case "cg none"  : cls = 4; 
                        break;
    
      case "cgs asm": cls = 5; 
                         break;
      case "cgs bjacobi": cls = 6; 
                         break;
      case "cgs sor"   : cls = 7; 
                         break;
      case "cgs none"  : cls = 8; 
                         break;

      case "bcgs asm": cls = 9; 
                          break;
      case "bcgs bjacobi": cls = 10; 
                          break;
      case "bcgs sor"   : cls = 11; 
                          break;
      case "bcgs none"  : cls = 12; 
                          break;

      case "bicg asm": cls = 13; 
                          break;
      case "bicg bjacobi": cls = 14; 
                          break;
      case "bicg none"  : cls = 15; 
                          break;

      case "tfqmr asm": cls = 16; 
                           break;
      case "tfqmr bjacobi": cls = 17; 
                           break;
      case "tfqmr sor"   : cls = 18; 
                           break;
      case "tfqmr none"  : cls = 19; 
                           break;

      case "gmres asm": cls = 20; 
                           break;
      case "gmres bjacobi": cls = 21; 
                           break;
      case "gmres sor"   : cls = 22; 
                           break;
      case "gmres none"  : cls = 23; 
                           break;      

      case "fgmres asm": cls = 24; 
                            break;
      case "fgmres bjacobi": cls = 25; 
                            break;
      case "fgmres sor"   : cls = 26; 
                            break;
      case "fgmres none"  : cls = 27; 
                            break;
    }
    return cls;
  }

  static void downloadProperties(String path) throws Exception{
    File dir = new File(path);
    File[] files = dir.listFiles();
    PrintWriter out = new PrintWriter(new BufferedWriter(new FileWriter(path+"matlab/target_class.txt")));
    StringBuilder sb = new StringBuilder("");

    int counter = 1;
    Arrays.sort(files);
    String l;

    for(int i = 0; i < 1; i++){
      BufferedReader br = null;
      if(files[i].getName().contains(".txt")){
        System.out.println(counter+" "+files[i].getName());
        br = new BufferedReader(new FileReader(path+files[i].getName()));

        l = br.readLine();        
        if(!l.contains("%"))System.out.println("Missing link: "+files[i].getName());
        else{
          URL url;
          InputStream is = null;
          String line;

          url = new URL(l.replace("% ",""));
          is = url.openStream();
          br = new BufferedReader(new InputStreamReader(is));
          while ((line = br.readLine()) != null) {
              System.out.println(line);
          }
          is.close();
        }
        br.close();        
      }
    }  
  }

  static void generateRandomTargetClassVector(){
    for(int i = 0; i < 960; i++){
      int r = 1 + (int)(Math.random() * ((42 - 1) + 1));
      System.out.print(r+" ");
    }
  }

  static void generateTargetClassVector(String path){
    try{
      File dir = new File(path);
      File[] files = dir.listFiles();
      PrintWriter out = new PrintWriter(new 
        BufferedWriter(new FileWriter("target_class.txt")));
      StringBuilder sb = new StringBuilder("Tc = [");

      int counter = 1;
      int ct = 0;
      Arrays.sort(files);
      for(int i = 0; i < files.length; i++){
        BufferedReader br = null;
        if(files[i].getName().contains(".txt")){
          if(i%2 ==0)
            System.out.println(counter+" "+files[i].getName());
          br = new BufferedReader(new FileReader(path+files[i].getName()));
          String[] s;
          String l = "";
          double c,t,r;
          String ksp,pc;
          double lowest = Double.MAX_VALUE;
          int cls = 42;
          l = br.readLine();
          while(l != null){
            if(l.contains("%")){
              l = br.readLine();
            }
            l = l.replace("Failed","-99");
            l = l.replace("|",";");
            s = l.split(";");
            c = Double.parseDouble(s[2]);
            if(c >= 0){
              t = Double.parseDouble(s[3]);
              if(t < lowest){
                lowest = t;                
              }
            }
           l = br.readLine();
          }
          br.close();
          br = new BufferedReader(new FileReader(path+files[i].getName()));
          l = br.readLine();
          while(l != null){
            if(l.contains("%")){
              l = br.readLine();
            }
            l = l.replace("Failed","-99");
            l = l.replace("|",";");
            s = l.split(";");
            c = Double.parseDouble(s[2]);
            if(c >= 0){
              t = Double.parseDouble(s[3]);
              if(t/lowest < 1.1) 
                sb.append("1 ");
              else sb.append("2 ");               
            }
            else{ 
              sb.append("2 ");
            }
            ct++;
            l = br.readLine();
          }
          //if(ct != 42) System.out.println(i+"."+files[i].getName());
          //sb.append(cls+" ");
          br.close();
          counter++;
        }        
      }
      System.out.println(ct);      
      sb.append("];\n");
      out.write(sb.toString());
      out.close();
    }catch(Exception x){
      x.printStackTrace();
    }    
  }

  static int getCls(String ksp, String pc){
    String m = ksp + " " + pc;
    int cls = 42;
    switch(m){
      case "cg ilu(0)": cls = 1; 
                        break;
      case "cg ilu(1)": cls = 2; 
                        break;
      case "cg ilu(2)": cls = 3; 
                        break;
      case "cg jacobi": cls = 4; 
                        break;
      case "cg sor"   : cls = 5; 
                        break;
      case "cg none"  : cls = 6; 
                        break;

      case "cgs ilu(0)": cls = 7; 
                         break;
      case "cgs ilu(1)": cls = 8; 
                         break;
      case "cgs ilu(2)": cls = 9; 
                         break;
      case "cgs jacobi": cls = 10; 
                         break;
      case "cgs sor"   : cls = 11; 
                         break;
      case "cgs none"  : cls = 12; 
                         break;

      case "bcgs ilu(0)": cls = 13; 
                          break;
      case "bcgs ilu(1)": cls = 14; 
                          break;
      case "bcgs ilu(2)": cls = 15; 
                          break;
      case "bcgs jacobi": cls = 16; 
                          break;
      case "bcgs sor"   : cls = 17; 
                          break;
      case "bcgs none"  : cls = 18; 
                          break;

      case "bicg ilu(0)": cls = 19; 
                          break;
      case "bicg ilu(1)": cls = 20; 
                          break;
      case "bicg ilu(2)": cls = 21; 
                          break;
      case "bicg jacobi": cls = 22; 
                          break;
      case "bicg none"  : cls = 23; 
                          break;

      case "tfqmr ilu(0)": cls = 24; 
                           break;
      case "tfqmr ilu(1)": cls = 25; 
                           break;
      case "tfqmr ilu(2)": cls = 26; 
                           break;
      case "tfqmr jacobi": cls = 27; 
                           break;
      case "tfqmr sor"   : cls = 28; 
                           break;
      case "tfqmr none"  : cls = 29; 
                           break;

      case "gmres ilu(0)": cls = 30; 
                           break;
      case "gmres ilu(1)": cls = 31; 
                           break;
      case "gmres ilu(2)": cls = 32; 
                           break;
      case "gmres jacobi": cls = 33; 
                           break;
      case "gmres sor"   : cls = 34; 
                           break;
      case "gmres none"  : cls = 35; 
                           break;      

      case "fgmres ilu(0)": cls = 36; 
                            break;
      case "fgmres ilu(1)": cls = 37; 
                            break;
      case "fgmres ilu(2)": cls = 38; 
                            break;
      case "fgmres jacobi": cls = 39; 
                            break;
      case "fgmres sor"   : cls = 40; 
                            break;
      case "fgmres none"  : cls = 41; 
                            break;
    }
    return cls;
  }

  static void generateMatlabScript(String path){
    try{
      File dir = new File(path);
      File[] files = dir.listFiles();
      PrintWriter out = new PrintWriter(new 
        BufferedWriter(new FileWriter("prop_matrix.txt")));
      StringBuilder sb = new StringBuilder("");
      int counter = 1;

      Arrays.sort(files);

      for(int i = 0; i < files.length; i++){
        BufferedReader br = null;
        if(files[i].getName().contains(".txt")){
          System.out.println(counter+" "+files[i].getName());
          br = new BufferedReader(new FileReader(path+files[i].getName()));
          String s[] = null;
          String l = "";
          double v;
          String ns = "";
          String wt = "";
          //sb.append("D("+counter+",:)=[");        
          l = br.readLine();
          while(l != null){
            s = l.split(":");
            v = Double.parseDouble(s[1]);
            //System.out.println(v);
            ns = ns + (v+" ");
            //sb.append(v+" ");
            l = br.readLine();
          }
          // for(int j=1; j <= 41; j++){
          //   //wt = wt + "D("+counter+",:)=[" + ns + j +"];\n";
          //   counter++;
          // }
          wt = "D("+counter+",:)=[" + ns + "];\n";
          counter++;
          sb.append(wt);
          br.close();
          //counter++;
        }        
      }
      out.write(sb.toString());
      out.close();
    }catch(Exception x){
      x.printStackTrace();
    }    
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
  	path = "/home/javed/Desktop/matrix_properties/matrices/";
    File dir = new File(path);
    File[] files = dir.listFiles();
    
    Arrays.sort(files, new Comparator<File>(){
      public int compare(File f1, File f2){
        return Long.valueOf(f1.length()).compareTo(f2.length());
      }
    });
    for(int i = 0; i < files.length; i++){
      if(files[i].getName().contains(".mtx")){
        //System.out.println("echo \""+i+"/"+files.length+"\"");
        System.out.println("./properties -f "
          + path+files[i].getName());
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