//NA 2017/2018: Zadaća 3, Zadatak 1
#include <iostream>
#include <cmath>
#include <algorithm>
#include <utility>
#include <vector>
#include <stdexcept>

double prava(std::pair<double,double> a, std::pair<double,double> b, double x)
{
    return ((b.first-x)/(b.first-a.first))*a.second + ((x-a.first)/(b.first-a.first))*b.second;
}

bool JesuLiJednaki(double x, double y, double Eps = 1e-10)
{
    return std::fabs(x-y)<=Eps*(std::fabs(x)+std::fabs(y));
}

int binaryS(const std::vector<std::pair<double,double>> &v,double x, int l, int r)
{
    while (l<=r) {
        int m = (r+l)/2;
        if(v[m].first<x && v[m+1].first>=x) return m;
        else if(v[m+1].first>x && v[m].first>=x) return binaryS(v,x,l,m-1);
        else return binaryS(v,x,m+1,r);
    }
    return 0; //nema elementa
}

class AbstractInterpolator
{
protected:
    std::vector<std::pair<double, double>> v;
    mutable int i=1;
    int Locate(double x) const {
        if(x<=v[0].first) return 0;
        if(x>v[v.size()-1].first) return v.size();
        if(x>=v[i-1].first && x<v[i].first) return i;
        i=binaryS(v,x,0,v.size()-1)+1;
        return i;
    }
public:
    AbstractInterpolator(const std::vector<std::pair<double, double>> &data) {
        v = data;
        std::sort(v.begin(), v.end(), [](std::pair<double,double> a, std::pair<double,double> b) {
            if(a.first<b.first) return true;
            return false;
        });
        for(int i(0); i<data.size()-1; i++) {
            if(JesuLiJednaki(data[i].first,data[i+1].first)) throw std::domain_error("Invalid data set");
        }

    }
    virtual double operator()(double x) const = 0;
};

class LinearInterpolator : public AbstractInterpolator
{
public:
    LinearInterpolator(const std::vector<std::pair<double, double>> &data) : AbstractInterpolator(data) {}
    double operator()(double x) const override {
        int i=Locate(x);
        if(i<=1) return prava(v[0],v[1],x);
        if(i>=v.size()) return prava(v[v.size()-2],v[v.size()-1],x);
        return prava(v[i-1],v[i],x);
    }
};

class PolynomialInterpolator : public AbstractInterpolator
{
private:
    std::vector<double> koeficijenti;
public:
    PolynomialInterpolator(const std::vector<std::pair<double, double>> &data) : AbstractInterpolator(data) {
        koeficijenti.resize(v.size());
        koeficijenti[0]=v[v.size()-1].second;
        for(int j(1); j<v.size(); j++) {
            for(int i(v.size()); i>=j+1; i--) {
                v[i-1].second = (v[i-1].second-v[i-2].second)/(v[i-1].first-v[i-j-1].first);
            }
            koeficijenti[j]=v[v.size()-1].second;
        }
    }
    double operator()(double x) const override {
        double f(1), s(v[0].second);
        for(int i(1); i<v.size(); i++) {
            f=f*(x-v[i-1].first);
            s+=v[i].second*f;
        }
        return s;
    }
    void AddPoint(const std::pair<double, double> &p) {
        for(int i(0); i<v.size(); i++) {
            if(JesuLiJednaki(p.first,v[i].first)) throw std::domain_error("Invalid point");
        }
        v.push_back(p);
        int n(v.size());
        koeficijenti.resize(n);
        for(int i(1); i<n; i++) {
            double temp=koeficijenti[i-1];
            koeficijenti[i-1]=v[n-1].second;
            v[n-1].second=(v[n-1].second-temp)/(v[n-1].first-v[n-i-1].first);
        }
        koeficijenti[n-1]=v[n-1].second;
    }
    std::vector<double> GetCoefficients() const {
        int n(v.size());
        std::vector<double> p(v.size()+1), w(v.size()+1);
        w[0]=1;
        for(int i(1); i<=n; i++) {
            for(int j(0); j<=i; j++)
                p[j]+=v[i-1].second*w[j];
            w[i]=w[i-1];
            for(int j(i-1); j>0; j--)
                w[j]=w[j-1]-v[i-1].first*w[j];
            w[0]*=(-1)*v[i-1].first;
        }
        p.erase(p.begin()+p.size()-1);
        return p;
    }
};


class SplineInterpolator : public AbstractInterpolator
{
    std::vector<double> r,q,s;
public:
    SplineInterpolator(const std::vector<std::pair<double, double>> &data) : AbstractInterpolator(data) {
        r.resize(v.size());
        s.resize(v.size());
        r[0]=0;
        r[r.size()-1]=0;
        for(int i(1); i<v.size()-1; i++) {
            s[i]=2*(v[i+1].first-v[i-1].first);
            r[i]=3*((v[i+1].second-v[i].second)/(v[i+1].first-v[i].first)-(v[i].second-v[i-1].second)/(v[i].first-v[i-1].first));
        }
        for(int i(1); i<v.size()-2; i++) {
            double mi=(v[i+1].first-v[i].first)/s[i];
            s[i+1]-=mi*(v[i+1].first-v[i].first);
            r[i+1]-=mi*r[i];
        }
        r[v.size()-2]/=s[v.size()-2];
        for(int i(v.size()-3); i>=1; i--)
            r[i] = (r[i]-(v[i+1].first-v[i].first)*r[i+1])/s[i];
        q.resize(v.size());
        for(int i(0); i<v.size()-1; i++) {
            double x(v[i+1].first-v[i].first);
            s[i]=(r[i+1]-r[i])/(3*x);
            q[i]=(v[i+1].second-v[i].second)/x-x*(r[i+1] + 2*r[i])/3;
        }
    }
    double operator()(double x) const override {
        int i=Locate(x);
        if(i<=0) i=1;
        if(i>=v.size()) i=v.size()-1;
        double t(x-v[i-1].first);
        return v[i-1].second+t*(q[i-1]+t*(r[i-1]+s[i-1]*t));
    }
};

class PiecewisePolynomialInterpolator : public AbstractInterpolator
{
    int k;
public:
    PiecewisePolynomialInterpolator(const std::vector<std::pair<double, double>> &data, int order) : AbstractInterpolator(data) {
        if(order<1 || order>data.size()) throw std::domain_error("Invalid order");
        k=order;
    }
    double operator()(double x) const override {
        int i=Locate(x),j,n;
        if(k%2==0) {
            j=i-k/2 - 1;
            n=i+k/2;
        } else {
            j=i-(k-1)/2 - 1;
            n=i+(k+1)/2;
        }
        if(j<=0) {
            j=0;
            n=k+1;
        }
        if(n>=v.size()) {
            j=v.size()-k - 1;
            n=v.size();
        }
        double s(0);
        for(int l(j); l<n; l++) {
            double p=v[l].second;
            for(int o(j); o<n; o++) {
                if(l!=o) p*=(x-v[o].first)/(v[l].first-v[o].first);
            }
            s+=p;
        }
        return s;
    }

};
class BarycentricInterpolator : public AbstractInterpolator
{
    std::vector<double> w;
    int d;
public:
    BarycentricInterpolator(const std::vector<std::pair<double, double>> &data, int order) : AbstractInterpolator(data) {
        if(order<0 || order>v.size()) throw std::domain_error("Invalid order");
        d=order;
        int n(v.size());
        w.resize(n);
        for(int i(0); i<v.size(); i++) {
            w[i]=0;
            double p;
            int m,l;
            if(i-d>1) m=i-d;
            else m=1;
            if(i<n-d) l=i;
            else l=n-d-1;
            for(int k(m-1); k<l+1; k++) {
                p=1;
                for(int j(k); j<k+d; j++)
                    if(j!=i)
                        p/=(v[i].first-v[j].first);
                if(k%2==1) p*=(-1);
            }
            w[i]+=p;
        }
    }
    double operator()(double x) const override {
        double p(0), q(0);
        for(int i(0); i<v.size(); i++) {
            if(JesuLiJednaki(x,v[i].first)) return v[i].second;
            double u=w[i]/(x-v[i].first);
            p+=u*v[i].second;
            q+=u;
        }
        return p/q;
    }
    std::vector<double> GetWeights() const {
        return w;
    }
};

template <typename FunTip>
double Limit(FunTip f, double x0, double h = 0, double eps = 1e-8, double nmax = 20)
{
    if(eps<=0 || nmax<3 || nmax>30) throw std::domain_error("Invalid parameters");
    if(std::abs(h)<eps) {
        if(x0>1) h=std::abs(x0)*0.001;
        else h=0.001;
    }
    std::vector<double> y(nmax,0);
    double p;
    double yinf(std::numeric_limits<double>::infinity());
    for(int i(0); i<nmax; i++) {
        y[i]=f(x0+h);
        p=2;
        for(int k(i-1); k>=0; k--) {
            y[k]=(p*y[k+1]-y[k])/(p-1);
            p*=2;
        }
        if(std::fabs(y[0]-yinf)<eps) return y[0];
        yinf=y[0];
        h/=2;
    }
    throw std::logic_error("Accuracy goal is not achieved");
}


int main ()
{
    {
        LinearInterpolator linear({{4,5},{6,7},{7,8},{8,10}});
        std::cout << linear(0.2) << std::endl;
        std::cout << linear(2) << std::endl;
        std::cout << linear(1.5) << std::endl;
        std::cout << linear(7.5) << std::endl;

    }
    {
        LinearInterpolator linear({{3,3},{4,4},{1,1},{6,6},{2,2},{5,5}});
        std::cout << linear(0.5) << std::endl;
        std::cout << linear(1) << std::endl;
        std::cout << linear(8) << std::endl;
        std::cout << linear(-0.1) << std::endl;
        std::cout << linear(3)<<std::endl;
    }
    {
        try {
            LinearInterpolator linear({{1,1},{1,1}});
            //Invalid data set
        } catch (std::domain_error e) {
            std::cout << "'" << e.what() << "'";
        } catch (...) {
            std::cout << "Pogresan tip izuzetka";
        }
        std::cout<<std::endl;
    }
    {
        PolynomialInterpolator p({{-1,-1},{0,0},{1,1},{2,2},{3,3}});
        std::vector<double> p2 = p.GetCoefficients();
        for(int i = 0; i < p2.size(); i++) std::cout << p2[i] << " ";
    }
    {
        const double PI=std::atan(1)*4;
        std::vector<std::pair<double,double>> data;
        for(double i=2*PI; i>=0; i-=PI/2)
            data.push_back({i,std::cos(i)});
        SplineInterpolator cosinus(data);
        for (int i=-PI/4; i<=2*PI/2; i++)
            std::cout<<cosinus(i)<<" "<<std::cos(i)<<std::endl;
    }
    {
        BarycentricInterpolator bi({{1,1},{2,2},{3,3},{4,4}}, 0);
        std::cout << bi(2.5) << std::endl;
        std::cout << bi(3.5) << std::endl;
        std::cout << bi(7.5) << std::endl;
        std::cout << bi(-3.5)<<std::endl;
    }
    {

        std::cout<<Limit([](double x) {
            return (std::exp(x)-1)/x;
        },0);
    }
    {
        std::cout<<Limit([](double x) {
            return (std::sin(x)-x)/x;
        },0);
    }
    {
        PiecewisePolynomialInterpolator p({{0, 8}, {1, 3}, {2, 7}, {4, 5}, {6, 8}, {8, 7}, {10, 5}}, 3);
        for(double x : {
                    -1., 0.5, 1.5, 3., 5., 7., 9., 10.5
                }) std::cout << p(x) << " ";
    }
    {
        PolynomialInterpolator p({{1,1},{2,2},{3,3}});
        p.AddPoint({4,4});
        std::cout << p(1.5)<<" " << p(4.5) << std::endl;
    }
    {

    }
    return 0;
}