//NA 2017/2018: Zadaća 2, Zadatak 1
#include <iostream>
#include <cmath>
#include <stdexcept>
#include <algorithm>
#include <limits>
#include <vector>
#include <iomanip>

bool JesuLiJednaki(double x, double y, double Eps = 1e-10) {
    return std::fabs(x-y)<=Eps*(std::fabs(x)+std::fabs(y));
}

class Vector
{
private:
    std::vector<double> vek;
public:
    explicit Vector(int n) {
        if(n<=0) throw std::range_error("Bad dimension");
        vek.resize(n);
        for(int i(0); i<n; i++) vek[i]=0;
    }
    Vector(std::initializer_list<double> l) {
        if(l.size() == 0) throw std::range_error("Bad dimension");
        for(double el : l) vek.push_back(el);
    }
    int NElems() const {
        return vek.size();
    }
    void Chop(double eps=-1) {
        if(eps<0) eps=GetEpsilon();
        for(int i(0); i<vek.size(); i++) 
            if(vek[i]<eps) vek[i]=0;
    }
    bool EqualTo(const Vector &v, double eps = -1) const {
        if(eps<0) eps=GetEpsilon();
        if(v.vek.size() == vek.size()) {
            for(int i(0); i<vek.size(); i++) {
                if((vek[i]-v[i])>eps) return false; 
            }
            return true;
        }
        return false;
    }
    double &operator[](int i) {
        if(i<0 || i> vek.size()-1) throw std::range_error("Invalid index");
        return vek[i];
    }
    double operator[](int i) const {
        if(i<0 || i> vek.size()-1) throw std::range_error("Invalid index");
        return vek[i];
    }
    double &operator()(int i) {
        if(i<1 || i> vek.size()) throw std::range_error("Invalid index");
        return vek[i-1];
    }
    double operator()(int i) const {
        if(i<1 || i> vek.size()) throw std::range_error("Invalid index");
        return vek[i-1];
    }
    double Norm() const {
        double suma(0);
        for(int i(0); i<vek.size(); i++) {
            suma+=std::pow(vek[i],2);
        }
        return std::sqrt(suma);
    }
    friend double VectorNorm(const Vector &v) {
        double suma(0);
        for(int i(0); i<v.vek.size(); i++) {
            suma+=std::pow(v[i],2);
        }
        return std::sqrt(suma);
    }
    double GetEpsilon() const {
        return 10*Norm() * std::numeric_limits<double>::epsilon();
    }
    void Print(char separator = '\n', double eps = -1) const {
        if(separator=='\n') {
            for(int i(0); i<vek.size(); i++) {
                if(eps<0) {
                    if(std::abs(vek[i]) < GetEpsilon()) std::cout<<"0"<<separator;
                    else std::cout<<vek[i]<<separator;
                } else {
                    if(std::abs(vek[i]) < eps) std::cout<<"0"<<separator;
                    else std::cout<<vek[i]<<separator;
                }
            }
        } else {
            for(int i(0); i<vek.size(); i++) {
                if(i==vek.size()-1) {
                    if(std::abs(vek[i]) < GetEpsilon()) std::cout<<"0";
                    else std::cout<<vek[i];
                } else {
                    if(eps<0) {
                        if(std::abs(vek[i]) < GetEpsilon()) std::cout<<"0"<<separator;
                        else std::cout<<vek[i]<<separator;
                    } else {
                        if(std::abs(vek[i]) < eps) std::cout<<"0"<<separator;
                        else std::cout<<vek[i]<<separator;
                    }
                }
            }
        }
    }
    friend void PrintVector(const Vector &v, char separator = '\n', double eps = -1) {
        if(separator=='\n') {
            for(int i(0); i<v.vek.size(); i++) {
                if(eps<0) {
                    if(std::abs(v.vek[i]) < v.GetEpsilon()) std::cout<<"0"<<separator;
                    else std::cout<<v.vek[i]<<separator;
                } else {
                    if(std::abs(v.vek[i]) < eps) std::cout<<"0"<<separator;
                    else std::cout<<v.vek[i]<<separator;
                }
            }
        } else {
            for(int i(0); i<v.vek.size(); i++) {
                if(i==v.vek.size()-1) {
                    if(std::abs(v.vek[i]) < v.GetEpsilon()) std::cout<<"0";
                    else std::cout<<v.vek[i];
                } else {
                    if(eps<0) {
                        if(std::abs(v.vek[i]) < v.GetEpsilon()) std::cout<<"0"<<separator;
                        else std::cout<<v.vek[i]<<separator;
                    } else {
                        if(std::abs(v.vek[i]) < eps) std::cout<<"0"<<separator;
                        else std::cout<<v.vek[i]<<separator;
                    }
                }
            }
        }
    }
    friend Vector operator +(const Vector &v1, const Vector &v2) {
        if(v1.vek.size() != v2.vek.size()) throw std::domain_error("Incompatible formats");
        Vector rez(v1.vek.size());
        for(int i(0); i<v1.vek.size(); i++) {
            rez[i] = v1[i]+v2[i];
        }
        return rez;
    }
    Vector &operator +=(const Vector &v) {
        if(vek.size() != v.vek.size()) throw std::domain_error("Incompatible formats");
        for(int i(0); i<vek.size(); i++) {
            vek[i]= vek[i]+v[i];
        }
        return *this;
    }
    friend Vector operator -(const Vector &v1, const Vector &v2) {
        if(v1.vek.size() != v2.vek.size()) throw std::domain_error("Incompatible formats");
        Vector rez(v1.vek.size());
        for(int i(0); i<v1.vek.size(); i++) {
            rez[i] = v1[i]-v2[i];
        }
        return rez;
    }
    Vector &operator -=(const Vector &v) {
        if(vek.size() != v.vek.size()) throw std::domain_error("Incompatible formats");
        for(int i(0); i<vek.size(); i++) {
            vek[i]-=v[i];
        }
        return *this;
    }
    friend Vector operator *(double s, const Vector &v) {
        Vector rez(v.vek.size());
        for(int i(0); i<v.vek.size(); i++) rez[i] = s*v[i];
        return rez;
    }
    friend Vector operator *(const Vector &v, double s) {
        Vector rez(v.vek.size());
        for(int i(0); i<v.vek.size(); i++) rez[i] = s*v[i];
        return rez;
    }
    Vector &operator *=(double s) {
        for(int i(0); i<vek.size(); i++) {
            vek[i]*=s;
        }
        return *this;
    }
    friend double operator *(const Vector &v1, const Vector &v2) {
        if(v1.vek.size() != v2.vek.size()) throw std::domain_error("Incompatible formats");
        double suma=0;
        for(int i(0); i<v1.vek.size(); i++) {
            suma+=v1[i]*v2[i];
        }
        return suma;
    }
    friend Vector operator /(const Vector &v, double s) {
        if(JesuLiJednaki(s,0)) throw std::domain_error("Division by zero");
        Vector rez(v.vek.size());
        for(int i(0); i<v.vek.size(); i++) rez[i] = v[i]/s;
        return rez;
    }
    Vector &operator /=(double s) {
        if(JesuLiJednaki(s,0)) throw std::domain_error("Division by zero");
        for(int i(0); i<vek.size(); i++) {
            vek[i]/=s;
        }
        return *this;
    }
};

class Matrix
{

    std::vector<std::vector<double>> mat;
    int RREFAndRank();
public:
    Matrix(int m, int n) {
        if(m<=0 || n<=0) throw std::range_error("Bad dimension");
        mat.resize(m);
        for(int i=0; i<m; i++) {
            mat[i].resize(n);
            for(int j=0; j<n; j++) mat[i][j] = 0;
        }
    }
    Matrix(const Vector &v) {
        if(v.NElems()) throw std::range_error("Bad dimension");
        mat.resize(1);
        mat[0].resize(v.NElems());
        for(int i(0); i<v.NElems(); i++)  mat[0][i]=v[i];
    }
    Matrix(std::initializer_list<std::vector<double>> l) {
        if(l.size()==0) throw std::range_error("Bad dimension");
        int vel = l.begin()->size();
        for(std::vector<double> el : l) {
            if(el.empty()) throw std::range_error("Bad dimension");
            if(vel != el.size()) throw std::logic_error("Bad matrix");
        }
        for(std::vector<double> el : l) {
            mat.push_back(el);
        }
    }
    int NRows() const {
        return mat.size();
    }
    int NCols() const {
        return mat[0].size();
    }
    double *operator[](int i) {
        return &mat[i][0];
    }
    const double *operator[](int i) const {
        return &mat[i][0];
    }
    double &operator()(int i, int j) {
        if(i<=0 || j<=0 || i>mat.size() || j>mat[0].size()) throw std::range_error("Invalid index");
        return mat[i-1][j-1];
    }
    double operator()(int i, int j) const {
        if(i<=0 || j<=0 || i>mat.size() || j>mat[0].size()) throw std::range_error("Invalid index");
        return mat[i-1][j-1];
    }
    double Norm() const {
        double suma=0;
        for(int i=0; i<mat.size(); i++)
            for(int j=0; j<mat[0].size(); j++)
                suma+=std::pow(mat[i][j],2);
        return std::sqrt(suma);
    }
    friend double MatrixNorm(const Matrix &m) {
        double suma=0;
        for(int i=0; i<m.mat.size(); i++)
            for(int j=0; j<m.mat[0].size(); j++)
                suma+=std::pow(m[i][j],2);
        return std::sqrt(suma);
    }
    double GetEpsilon() const {
        return 10*Norm()*std::numeric_limits<double>::epsilon();
    }
    void Print(int width = 10, double eps = -1) const {
        for(int i(0); i<mat.size(); i++) {
            for(int j(0); j<mat[i].size(); j++) {
                if(eps<0) {
                    if(std::abs(mat[i][j]) < GetEpsilon()) std::cout << std::setw(width) << "0";
                    else std::cout << std::setw(width) << mat[i][j];
                } else {
                    if(std::abs(mat[i][j]) < eps) std::cout << std::setw(width) << "0";
                    else std::cout << std::setw(width) << mat[i][j];
                }
            }
            std::cout << std::endl;
        }
    }
    friend void PrintMatrix(const Matrix &m, int width = 10, double eps = -1) {
        for(int i(0); i<m.mat.size(); i++) {
            for(int j(0); j<m.mat[i].size(); j++) {
                if(eps<0) {
                    if(std::abs(m.mat[i][j]) < m.GetEpsilon()) std::cout << std::setw(width) << "0";
                    else std::cout << std::setw(width) << m.mat[i][j];
                } else {
                    if(std::abs(m.mat[i][j]) < eps) std::cout << std::setw(width) << "0";
                    else std::cout << std::setw(width) << m.mat[i][j];
                }
            }
            std::cout << std::endl;
        }
    }
    friend Matrix operator +(const Matrix &m1, const Matrix &m2) {
        if(m1.NRows() != m2.NRows() || m1.NCols() != m2.NCols()) throw std::domain_error("Incompatible formats");
        Matrix rez(m1.NRows(),m1.NCols());
        for(int i=0; i<m1.NRows(); i++)
            for(int j=0; j<m1.NCols(); j++)
                rez[i][j] = m1[i][j]+m2[i][j];
        return rez;
    }
    Matrix &operator +=(const Matrix &m) {
        if(NRows() != m.NRows() || NCols() != m.NCols()) throw std::domain_error("Incompatible formats");
        for(int i=0; i<NRows(); i++)
            for(int j=0; j<NCols(); j++)
                mat[i][j] = mat[i][j]+m[i][j];
        return *this;
    }
    friend Matrix operator -(const Matrix &m1, const Matrix &m2) {
        if(m1.NRows() != m2.NRows() || m1.NCols() != m2.NCols()) throw std::domain_error("Incompatible formats");
        Matrix rez(m1.NRows(),m1.NCols());
        for(int i=0; i<m1.NRows(); i++)
            for(int j=0; j<m1.NCols(); j++)
                rez[i][j] = m1[i][j]-m2[i][j];
        return rez;
    }
    Matrix &operator -=(const Matrix &m) {
        if(NRows() != m.NRows() || NCols() != m.NCols()) throw std::domain_error("Incompatible formats");
        for(int i=0; i<NRows(); i++)
            for(int j=0; j<NCols(); j++)
                mat[i][j] = mat[i][j]-m[i][j];
        return *this;
    }
    friend Matrix operator *(double s, const Matrix &m) {
        Matrix rez(m.NRows(),m.NCols());
        for(int i=0; i<m.NRows(); i++)
            for(int j=0; j<m.NCols(); j++)
                rez[i][j] = m[i][j]*s;
        return rez;
    }
    friend Matrix operator *(const Matrix &m, double s) {
        Matrix rez(m.NRows(),m.NCols());
        for(int i=0; i<m.NRows(); i++)
            for(int j=0; j<m.NCols(); j++)
                rez[i][j] = m[i][j]*s;
        return rez;
    }
    Matrix &operator *=(double s) {
        for(int i=0; i<NRows(); i++)
            for(int j=0; j<NCols(); j++)
                mat[i][j] = mat[i][j]*s;
        return *this;
    }
    friend Matrix operator *(const Matrix &m1, const Matrix &m2) {
        if(m1.NCols() != m2.NRows()) throw std::domain_error("Incompatible formats");
        Matrix rez(m1.NRows(), m2.NCols());

        for(int i=0; i<m1.NRows(); i++)
            for(int j=0; j<m2.NCols(); j++)
                rez[i][j] = 0;

        for(int i=0; i<m1.NRows(); i++)
            for(int j=0; j<m2.NCols(); j++)
                for(int k=0; k<m1.NCols(); k++)
                    rez[i][j]+=m1[i][k]*m2[k][j];
        return rez;
    }
    Matrix &operator *=(const Matrix &m) {
        if(NCols() != m.NRows()) throw std::domain_error("Incompatible formats");

        Matrix rez(NRows(), m.NCols());

        for(int i=0; i<NRows(); i++)
            for(int j=0; j<m.NCols(); j++)
                rez[i][j] = 0;

        for(int i=0; i<NRows(); i++)
            for(int j=0; j<m.NCols(); j++)
                for(int k=0; k<NCols(); k++)
                    rez[i][j]+=mat[i][k]*m[k][j];

        for(int i=0; i<rez.NRows(); i++) {
            mat[i].resize(m.NCols());
            for(int j=0; j<rez.NCols(); j++)
                mat[i][j] = rez[i][j];
        }
        return *this;
    }
    friend Vector operator *(const Matrix &m, const Vector &v) {
        if(m.NCols() != v.NElems()) throw std::domain_error("Incompatible formats");
        Vector rez(m.NRows());
        for(int i=0; i<m.NRows(); i++) rez[i] = 0;
        for(int i=0; i<m.NRows(); i++)
            for(int j=0; j<m.NCols(); j++)
                rez[i]+=m[i][j]*v[j];
        return rez;
    }
    friend Matrix Transpose(const Matrix &m) {
        Matrix rez(m.NCols(), m.NRows());
        for(int i=0; i<m.NRows(); i++)
            for(int j=0; j<m.NCols(); j++)
                rez[j][i] = m[i][j];
        return rez;
    }
    void Transpose() {
        if(NCols() == NRows()) {
            for(int i(0); i<NRows(); i++) {
                for(int j(i); j<NRows(); j++) {
                    double temp = mat[i][j];
                    mat[i][j] = mat[j][i];
                    mat[j][i] = temp;
                }
            }
        }
        else{
        Matrix rez(NCols(), NRows());
        for(int i=0; i<NRows(); i++)
            for(int j=0; j<NCols(); j++)
                rez[j][i] = mat[i][j];
        mat.resize(rez.NRows());
        for(int i=0; i<rez.NRows(); i++) {
            mat[i].resize(rez.NCols());
            for(int j=0; j<rez.NCols(); j++)
                mat[i][j] = rez[i][j];
        }
    }
    }
    void Chop(double eps = -1) {
        if(eps<0) eps=GetEpsilon();
        for(int i(0); i<mat.size(); i++) {
            for(int j(0); j<mat[i].size(); j++) {
                if(mat[i][j] < eps) mat[i][j]=0;
            }
        }
    }
    bool EqualTo(const Matrix &m, double eps = -1) const {
        if(eps<0) eps=GetEpsilon();
        if(mat.size() == m.NRows() && mat[0].size() == m.NCols()) {
            for(int i(0); i<mat.size(); i++) {
                for(int j(0); j<mat[i].size(); j++) {
                    if((mat[i][j]-m[i][j])> eps) return false;
                }
            }
            return true;
        }
        return false;
    }
    friend Matrix LeftDiv(Matrix m1, Matrix m2) {
        if(m1.NRows() != m1.NCols()) throw std::domain_error("Divisor matrix is not square");
        if(m1.NRows() != m2.NRows()) throw std::domain_error("Incompatible formats");
        int n(m2.NRows());
        int m(m2.NCols());
        for(int k(0); k<n; k++) {
            int p=k;
            for(int i(k+1); i<n; i++) {
                if(std::fabs(m1[i][k])>std::fabs(m1[p][k])) p=i;
                }
            if(std::fabs(m1[p][k])<m1.GetEpsilon()) throw std::domain_error("Divisor matrix is singular");
            if(p!=k) {
                    
                    std::swap(m1.mat[k], m1.mat[p]);
                    std::swap(m2.mat[k], m2.mat[p]);
                    
                }
                
            for(int i(k+1); i<n; i++) {
                double mi = m1[i][k]/m1[k][k];
                for(int j(k+1); j<n; j++) m1[i][j] = m1[i][j]-mi*m1[k][j];
                for(int j(0); j<m; j++) m2[i][j] = m2[i][j]-mi*m2[k][j];
            }
        }
        Matrix rez(n,m);
        for(int k(0); k<m; k++) {
            for(int i(n-1); i>=0; i--) {
                double s=m2[i][k];
                for(int j(i+1); j<n; j++) {
                    s=s-m1[i][j]*rez[j][k];
                }
                rez[i][k] = s/m1[i][i];
            }
        }
        return rez;
    }
    friend Vector LeftDiv(Matrix m, Vector v) {
        Matrix rez(LeftDiv(m,Matrix(v)));
        Vector v1(rez.NRows());
        for(int i(0); i<v1.NElems(); i++) {
            v1[i] = rez[i][0];
        }
        return v1;
    }
    friend Matrix operator /(const Matrix &m, double s) {
        if(JesuLiJednaki(s,0)) throw std::domain_error("Division by zero");
        Matrix rez(m.NRows(),m.NCols());
        for(int i=0; i<m.NRows(); i++)
            for(int j=0; j<m.NCols(); j++)
                rez[i][j] = m[i][j]/s;
        return rez;
    }
    Matrix &operator /=(double s) {
        if(JesuLiJednaki(s,0)) throw std::domain_error("Division by zero");
        for(int i=0; i<NRows(); i++)
            for(int j=0; j<NCols(); j++)
                mat[i][j] = mat[i][j]/s;
        return *this;
    }
    friend Matrix operator /(Matrix m1, Matrix m2) {
        return m1/=m2;
    }
    Matrix &operator /=(Matrix m1) {
        Matrix& m2(*this);
        if(m1.NRows() != m1.NCols()) throw std::domain_error("Divisor matrix is not square");
        if(m1.NRows() != m2.NRows()) throw std::domain_error("Incompatible formats");
        int n(m2.NRows());
        int m(m2.NCols());
        for(int k(0); k<n; k++) {
            int p=k;
            for(int i(k+1); i<n; i++) {
                if(std::fabs(m1[k][i])>std::fabs(m1[k][p])) p=i;
                }
            if(std::fabs(m1[k][p])<m1.GetEpsilon()) throw std::domain_error("Divisor matrix is singular");
            if(p!=k) {
                    for(int i(0); i<m1.NRows(); i++)
                    std::swap(m1[i][k], m1[i][p]);
                    for(int i(0); i<m2.NRows(); i++)
                    std::swap(m2[i][k], m2[i][p]);
                    
                }
                
            for(int i(k+1); i<n; i++) {
                double mi = m1[k][i]/m1[k][k];
                for(int j(k+1); j<n; j++) m1[j][i] = m1[j][i]-mi*m1[j][k];
                for(int j(0); j<m; j++) m2[j][i] = m2[j][i]-mi*m2[j][k];
            }
        }
        for(int k(0); k<m; k++) {
            for(int i(n-1); i>=0; i--) {
                double s=m2[k][i];
                for(int j(i+1); j<n; j++) {
                    s=s-m1[j][i]*m2[k][j];
                }
                m2[k][i] = s/m1[i][i];
            }
        }
        return *this;
    }
    double Det() const {
        if(mat.size() != mat[0].size()) throw std::domain_error("Matrix is not square");
        Matrix m(*this);
        int n(NCols());
        double d=1;
        for(int k(1); k<n; k++) {
            int p=k;
            for(int i(k+1); i<n; i++) {
                if(std::fabs(m[i][k])>std::fabs(m[p][k])) p=i;
            }
            if(std::fabs(m[p][k])<GetEpsilon()) return 0;
            if(p!=k) {
                std::swap(m.mat[p], m.mat[k]);
                d*=-1;
            }
            for(int i(k+1); i<n; i++) {
                double mi=m[i][k]/m[k][k];
                for(int j(k+1); j<n; j++) 
                    m[i][j]=m[i][j]-mi*m[k][j];
            }
        }
        for(int i(0); i<n; i++) d*=m[i][i];
        return d;
    }
    friend double Det(Matrix m) {
        return m.Det();
    }
    void Invert() {
        if(JesuLiJednaki(Det(),0)) throw std::domain_error("Matrix is singular");
        if(mat.size() != mat[0].size()) throw std::domain_error("Matrix is not square");
        int n(NCols());
        Matrix& m(*this);
        for(int k(0); k<n; k++) {
            double mi=m[k][k];
            m[k][k]=1;
            for(int j(0); j<n; j++) 
                m[k][j] = m[k][j] / mi;
            for(int i(0); i<n; i++) {
                if(i!=k) {
                    mi=m[i][k];
                    m[i][k]=0;
                    for(int j(0); j<n; j++)
                        m[i][j] = m[i][j] - mi*m[k][j];
                }
            }
        }
    }
    friend Matrix Inverse(Matrix m) {
        m.Invert();
        return m;
    }

    void ReduceToRREF() {
        RREFAndRank();
    }
    friend Matrix RREF(Matrix m) {
        m.ReduceToRREF();
        return m;
    }
    int Rank() const {
        Matrix pom(*this);
        return pom.RREFAndRank();
    }
    friend int Rank(Matrix m) {
        return m.Rank();
    }
};

    int Matrix::RREFAndRank() {
        Matrix& pom(*this);
        int k=-1, l=-1, p;
        int n(NRows());
        int m(NCols());
        while(k<m && l<n) {
            l++;
            k++;
            int v(0);
            while(v<GetEpsilon() && l<n) {
                p=k;
                for(int i=k; i<m; i++) {
                    if(std::fabs(pom[i][l])>v)
                    v=std::fabs(pom[i][l]);
                    p=i;
                }
                if(v<GetEpsilon()) l++;
            }
            if(l<NCols()) {
                if(p!=k) {
                    std::swap(pom.mat[k], pom.mat[p]);
                }
                double mi=pom[k][l];
                for(int j=l; j<n; j++) 
                    pom[k][j]/=mi;
                for(int i=0; i<m; i++)
                    if(i!=k) {
                        mi=pom[i][l];
                        for(int j=l; j<n; j++)
                            pom[i][j]-=mi*pom[k][j];
                    }
            }
        }
        return k;
    }

class LUDecomposer {
    Matrix m;
    Vector v;
    double test1(int i, int j) const {
        if(j>i) return 0;
        if(j==i) return 1;
        return m[i][j];
    }
    double test2(int i, int j) const {
        if(j<i) return 0;
        return m[i][j];
    }
    public:
    LUDecomposer(Matrix m1) : m(m1.NRows(), m1.NCols()), v(m1.NRows()){
        if(m1.NRows() != m1.NCols()) throw std::domain_error("Matrix is not square");
        int n(m1.NCols()), p;
        double s;
        m=m1;
        for(int j(0); j<n; j++) {
            for(int i(0); i<=j; i++) {
                s=m[i][j];
                for(int k(0); k<=i-1; k++) s=s-m[i][k]*m[k][j];
                m[i][j]=s;
            }
            p=j;
            for(int i=j+1;i<n;i++) {
                s=m[i][j];
                for(int k(0); k<=j-1; k++) s=s-m[i][k]*m[k][j];
                m[i][j]=s;
                if(std::fabs(s)>std::fabs(m[p][j])) p=i;
            }
            if(std::fabs(m[p][j])<m.GetEpsilon())
            throw std::domain_error("Matrix is singular");
            if(p!=j) {
                for(int i(0); i<n; i++) 
                    std::swap(m[j][i], m[p][i]);
            }
            v[j] = p;
            double mi= 1/m[j][j];
            for(int i=j+1;i<n;i++) 
                m[i][j] = mi * m[i][j];
        }
    }
    void Solve(const Vector &b, Vector &x) const {
        Vector a(b);
        if(a.NElems() != m.NRows() || a.NElems() != x.NElems()) 
            throw std::domain_error("Incompatible formats");
        int n(a.NElems());
        for(int i(0); i<n; i++) {
            int p(v[i]);
            double s(a[p]);
            a[p] = a[i];
            for(int j(0); j<=i-1; j++) s=s-test1(i,j)*x[j];
            x[i]=s;
        }
        for(int i(n-1); i>=0; i--) {
            double s=x[i];
            for(int j(i+1); j<n; j++) s=s-test2(i,j)*x[j];
            x[i] = s/test2(i,i);
        }
    }
    Vector Solve(Vector b) const {
        Vector pom(b.NElems());
        Solve(b,pom);
        return pom;
    }
    void Solve(Matrix &b, Matrix &x) const {
        if(b.NCols() != x.NCols()) throw std::domain_error("Incompatible formats");
        for(int i(0); i<b.NCols(); i++) {
            Vector b1(b.NRows());
            Vector x1(b.NRows());
            for(int j(0); j<b.NRows(); j++) 
                b1[j] = b[j][i];
                Solve(b1,x1);
                for(int j(0); j<b.NRows(); j++) 
                    x[j][i] = x1[j];
        }
    }
    Matrix Solve(Matrix b) const {
        Matrix pom(b.NRows(), b.NCols());
        Solve(b,pom);
        return pom;
    }
    Matrix GetCompactLU() const {
        return m;
    }
    Matrix GetL() const {
        Matrix rez(m.NRows(), m.NCols());
        for(int i(0); i<m.NRows(); i++) 
            for(int j(0); j<m.NCols(); j++) 
                rez[i][j] = test1(i,j);
        return rez;
    }
    Matrix GetU() const {
        Matrix rez(m.NRows(), m.NCols());
        for(int i(0); i<m.NRows(); i++) 
            for(int j(0); j<m.NCols(); j++) 
                rez[i][j] = test2(i,j);
        return rez;
    }
    Vector GetPermuation() const {
        return v;
    }
};


int main ()
{
    try {
        std::cout<<"Unesite broj elemenata prvog vektora : ";
        double broj1;
        std::cin>>broj1;
        Vector v1(broj1);
        std::cout<<"Unesite elemenate prvog vektora: ";
        for(int i(0); i<broj1; i++) {
            double temp;
            std::cin>>temp;
            v1[i] = temp;
        }
        std::cout<<"Unesite broj elemenata prvog vektora : ";
        double broj2;
        std::cin>>broj2;
        Vector v2(broj2);
        std::cout<<"Unesite elemenate prvog vektora: ";
        for(int i(0); i<broj2; i++) {
            double temp;
            std::cin>>temp;
            v2[i] = temp;
        }
        std::cout << VectorNorm(v1) <<" "<< v2.Norm() << std::endl;
        v1.Print();
        PrintVector(v2);

        v1*=5;
        v2*=2;
        v1.Print(2,',');
        v2.Print(3,',');
        Vector v(broj1);
        v=v1+v2;
        v.Print();
        Vector neki{1,2,3,4,5,6,7,8,9,10};
        neki=v*2;
        PrintVector(neki);
        v=v1-v2;
        v+=v2;
        v-=v2;
        v.Print();
        v1/=5;
        v2/=2;
        v1.Print(2,',');
        v2.Print(3,',');
        neki=v/2;
        PrintVector(neki);
        std::cout<<v1*v2<<std::endl;
        std::cout << neki.NElems();
    } catch(std::range_error izuzetak) {
        std::cout<<izuzetak.what();
    } catch(std::domain_error izuzetak) {
        std::cout<<izuzetak.what();
    }
    try {
        std::cout<<"Unesite dimenziju prve matrice: ";
        double m1,n1;
        std::cin>>m1;
        std::cin>>n1;
        Matrix mat1(m1,n1);
        std::cout<<"Unesite elemenate prve matrice: ";
        for(int i(0); i<m1; i++) {
            for(int j(0); j<n1; j++) {
                double temp;
                std::cin>>temp;
                mat1[i][j] = temp;
            }
        }
        std::cout<<"Unesite dimenziju druge matrice: ";
        double m2,n2;
        std::cin>>m2;
        std::cin>>n2;
        Matrix mat2(m2,n2);
        std::cout<<"Unesite elemenate prve matrice: ";
        for(int i(0); i<m2; i++) {
            for(int j(0); j<n2; j++) {
                double temp;
                std::cin>>temp;
                mat2[i][j] = temp;
            }
        }
        std::cout << MatrixNorm(mat1) <<" "<< mat2.Norm() << std::endl;
        mat1.Print();
        PrintMatrix(mat2);

        mat1*=5;
        mat2*=2;
        mat1.Print(2);
        mat2.Print(3);
        Matrix mat(m1,n1);
        mat=mat1+mat2;
        mat.Print();
        Matrix lol(m2,n2);
        lol=mat*2;
        PrintMatrix(lol);
        mat=mat1-mat2;
        mat.Print();
        Matrix proizvod = mat1*mat2;
        PrintMatrix(proizvod);
        Matrix transponovana = Transpose(mat);
        PrintMatrix(transponovana);
        std::cout << transponovana.NCols() << " " << transponovana.NRows() << std::endl;
        mat*=transponovana;
        PrintMatrix(mat);
    }
    catch(std::range_error izuzetak) {
        std::cout<<izuzetak.what();
    }
    catch(std::domain_error izuzetak) {
        std::cout<<izuzetak.what();
    }
     
    return 0;
}