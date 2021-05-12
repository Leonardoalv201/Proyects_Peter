#include<iostream>
#include<fstream>

using namespace std;

void espacios(int n_size){
    int n=40;
    n -= n_size;
    for(int i=0;i<n;i++){
        cout<<" ";
    }
}

bool letra(char token){
    if (token >= 65 && token <=90) return true;
    else if(token >= 97 && token <=122) return true;
    else return false;
}

bool numero(char token){
    if (token >= 48 && token <=57) return true;
    else return false;
}

bool comentario(char token, string line, int& i){
    if (token=='/' and line[i+1]=='/'){
        i=line.size();
        return true;
    }
    else{
        return false;
    }

}

bool decimal(char token, string line, int& i){
    int n=i;
    if(numero(token) == true || token == 45 || token == 46){
        while(numero(line[n]) == true || line[n] == 45 || line[n] == 46){
            if (line[n] == 46){
                return true;
            }
            n++;
        }
    }
    return false;
}

bool real(char token, string line, int& i){
    int length_real=0;
    if (decimal(token, line, i)==true){
        if(numero(token) == true || token == 45 || token == 46){
        while(numero(line[i]) == true || line[i] == 45 || line[i] == 46 || line[i] == 69 || line[i] == 101){
            cout << line[i];
            i++;
            length_real++;
        }

        espacios(length_real);
        cout << "Real" << endl;
        return true;
        }
    }
    else{
        return false;
    }
}

bool variable(char token, string line, int& i){
    int length_variable = 0;
    if(letra(token)==true){
        while (letra(line[i]) == true || numero(line[i]) == true || line[i] == '_'){
            cout << line[i];
            i++;
            length_variable++;
        }
        if(length_variable == 1){
            i--;
        }
        espacios(length_variable);
        cout << "Variable" << endl;
        return true;
    }
    else{
        return false;
    }

}

bool entero(char token, string line, int& i){
    int length_entero = 0;
    if(token==45){
        if (numero(line[i+1])==true){
            while(numero(line[i+1])==true){
                cout << line[i+1];
                i++;
                length_entero++;
            }
            if(length_entero == 1){
                i--;
            }
            espacios(length_entero);
            cout << "Entero" <<endl;
            return true;
        }
    }
    else if(numero(line[i])==true){
        while(numero(line[i])==true){
            cout << line[i];
            i++;
            length_entero++;
        }
        if(length_entero == 1){
            i--;
        }
        espacios(length_entero);
        cout << "Entero" <<endl;
        return true;
    }
    else{
        return false;
    }
}

bool operacion(char token){
    if(token == 40){
        cout << token;
        espacios(1);
        cout << "Parentesis que abre" <<endl;
        return true;
    }
    else if(token == 41){
        cout << token;
        espacios(1);
        cout << "Parentesis que cierra" <<endl;
        return true;
    }
    else if(token == 42){
        cout << token;
        espacios(1);
        cout << "Multiplicacion" <<endl;
        return true;
    }
    else if(token == 43){
        cout << token;
        espacios(1);
        cout << "Suma" <<endl;
        return true;
    }
    else if(token == 45){
        cout << token;
        espacios(1);
        cout << "Resta" <<endl;
        return true;
    }
    else if(token == 47){
        cout << token;
        espacios(1);
        cout << "Division" <<endl;
        return true;
    }
    else if(token == 94){
        cout << token;
        espacios(1);
        cout << "Potencia" <<endl;
        return true;
    }
    else if(token == 61){
        cout << token;
        espacios(1);
        cout << "Asignacion" <<endl;
        return true;
    }
    else{
        return false;
    }
}

bool espacios(char token){
    if (token == 32){
        return true;
    }
    else{
        return false;
    }
}

void token_no_valido(char token){
    cout << token;
    espacios(1);
    cout << "No es un input valido"<<endl;
    return;
}

void identificar_token(char token, string line, int& i){
    if(comentario(token, line, i)==true)return;
    else if(real(token, line, i)==true)return;
    else if(variable(token, line, i)==true)return;
    else if(entero(token, line, i)==true)return;
    else if(operacion(token)==true)return;
    else if(espacios(token)==true)return;
    else {
        token_no_valido(token);
        return;
    }
}

void read_line(string line){
    for(int i=0; i < line.size(); i++){
        identificar_token(line[i], line, i);
    }
}

int main(){
    string archivo;
    cout<<"Nombre del archivo: ";
    cin>>archivo;
    ifstream txt;
    string line;
    txt.open(archivo);
    cout<<"TOKEN";
    espacios(5);
    cout <<"TIPO"<<endl;
    while (!txt.eof()){
        getline(txt,line);
        read_line(line);
    }
    txt.close();
    return 0;
}
