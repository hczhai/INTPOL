#include <string>
#include <vector>
#include <set>
#include <map>
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <cmath>
#if defined(__WINDOWS__) || defined(_WIN32) || defined(WIN32) || defined(_WIN64) || defined(WIN64) || defined(__WIN32__) || defined(__TOS_WIN__)
#include <io.h>
#include <direct.h>
#else
#include <unistd.h>
#include <sys/stat.h>
#define _access access
#define _mkdir(x) mkdir(x, S_IRWXU)
#define _getcwd getcwd
#define _chmod chmod
#endif

using namespace std;

namespace fitting {

	class exception {
	public:
		string g;
	public:
		exception(const string& x);
	};
}

// 读取参数、基本数据转换、文件读写
namespace Parse {

	const string DoubleToString(const double d) {
		stringstream ss;
		ss.precision(10);
		ss << d;
		return ss.str();
	}

	const string IntToString(const int i) {
		stringstream ss;
		ss << i;
		return ss.str();
	}

	int StringToInt(const string& x) { return atoi(x.c_str()); }
	double StringToDouble(const string& x) { return atof(x.c_str()); }

	string Join(const set<string>& a, const string& x) {
		string r = "";
		for (set<string>::const_iterator i = a.begin(); i != a.end(); i++)
			r += *i + x;
		if (r.size() != 0) r.resize(r.size() - x.length());
		return r;
	}

	string Join(const vector<int>& a, const string& x) {
		string r = "";
		for (vector<int>::const_iterator i = a.begin(); i != a.end(); i++)
			r += IntToString(*i) + x;
		if (r.size() != 0) r.resize(r.size() - x.length());
		return r;
	}

	string Join(const vector<string>& a, const string& x) {
		string r = "";
		for (vector<string>::const_iterator i = a.begin(); i != a.end(); i++)
			r += *i + x;
		if (r.size() != 0) r.resize(r.size() - x.length());
		return r;
	}

	const string ToLower(const string& s) {
		string g = "";
		for (size_t i = 0; i < s.length(); i++)
			g += (s[i] >= 'a' && s[i] <= 'z') ? s[i] : (char)tolower(s[i]);
		return g;
	}

	const string ToUpper(const string& s) {
		string g = "";
		for (size_t i = 0; i < s.length(); i++)
			g += (s[i] >= 'a' && s[i] <= 'z') ? s[i] : (char)toupper(s[i]);
		return g;
	}

	const string Capital(const string& s) {
		if (s.length() == 0 || (s[0] >= 'A' && s[0] <= 'Z')) return s;
		return string("") + (char)toupper(s[0]) + s.substr(1);
	}


	bool MakeDirectory(const string& path) {
		return _mkdir(path.c_str()) == 0;
	}

	bool FileOrPathExists(const string& filename) {
		return _access(filename.c_str(), 6) == 0;
	}

	bool ChangeMode(const string& filename, int mode) {
		return _chmod(filename.c_str(), mode) == 0;
	}

	const string GetFileDirectory(const string& filename) {
		int cur = filename.length() - 1;
		for (; cur >= 0; cur--) {
			if (filename[cur] == '\\' || filename[cur] == '/')
				break;
		}
		if (cur >= 0) {
			if (cur == 0) return string(filename, 0, 1);
			else return string(filename, 0, cur);
		} else {
			char *buffer = _getcwd(NULL, 0);
			if (buffer == NULL)
				throw fitting::exception("Directory Error: Cannot get current work directory: \n" + filename);
			const string x = string(buffer);
			free(buffer);
			return x;
		}
	}

	void WriteFileContent(const string& filename, const string& cont) {
		const string cs = filename;
		ofstream ofs(cs.c_str());
		ofs.write(cont.c_str(), cont.length());
		ofs.close();
	}

	const string ReadFileContent(const string& filename) {
		ifstream file(filename.c_str());
		if (!file)
			throw fitting::exception("File Parse Error: Cannot open input file: \n" + filename);
		string h;
		string r = "";
		for (; !file.eof();) {
			getline(file, h);
			r += h + "\n";
		}
		file.close();
		return r;
	}

	struct MapSV { // short type name for a pair vector
		vector<pair<string, vector<string> > > mapsv;
		MapSV() : mapsv(vector<pair<string, vector<string> > >()) {}
		MapSV(const vector<pair<string, vector<string> > >& mapsv) : mapsv(mapsv) {}
		MapSV& operator=(const MapSV& sv) {
			if (this != &sv) this->mapsv = sv.mapsv;
			return *this;
		}
	};

	string& Trim(string& s) {
		if (s.empty()) return s;
		s.erase(0, s.find_first_not_of(" \t"));
		s.erase(s.find_last_not_of(" \t") + 1);
		return s;
	}

	// delim 可以包含多个字符，表示其中任意一个字符都作为分隔符
	vector<string> Split(const string& s, const string& delim) {
		vector<string> ret = vector<string>();
		size_t last = 0;
		size_t index = s.find_first_of(delim, last);
		while (index != string::npos) {
			ret.push_back(s.substr(last, index - last));
			last = index + 1;
			index = s.find_first_of(delim, last);
		}
		if (index - last > 0)
			ret.push_back(s.substr(last, index - last));
		return ret;
	}

	const vector<string> TrimRemoveEmpty(const vector<string>& vv) {
		vector<string> v = vv;
		for_each(v.begin(), v.end(), Trim);
		vector<string>::const_iterator pend = remove(v.begin(), v.end(), "");
		v.resize(pend - v.begin());
		return v;
	}

	const string ReadStream(istream *input) {
		string h;
		string r = "";
		for (; !input->eof();) {
			getline(*input, h);
			if (h == "EOF") break;
			size_t ind = h.find("!");
			if (ind != string::npos)
				h = string(h, 0, ind);
			while ((ind = h.find("\r")) != string::npos)
				h.replace(ind, 1, "");
			r += h + " ";
		}
		return r;
	}

	const vector<pair<string, MapSV> > RawParse(const string& x) {
		vector<pair<string, MapSV> > r;
		const vector<string>& progs = TrimRemoveEmpty(Split(x, "{}"));
		for (size_t i = 0; i < progs.size(); i++) {
			vector<string> ptbody = TrimRemoveEmpty(Split(progs[i], ":"));
			string title = "", body = "";
			if (ptbody.size() == 2)
				title = ptbody[0], body = ptbody[1];
			else if (ptbody.size() == 1)
				title = "parameter", body = ptbody[0];
			else {
				title = ptbody[0];
				ptbody.erase(ptbody.begin());
				body = Join(ptbody, ":");
			}
			const vector<string>& pterms = TrimRemoveEmpty(Split(body, ";"));
			vector<pair<string, vector<string> > > mterms;
			for (size_t j = 0; j < pterms.size(); j++) {
				const vector<string>& pterm = TrimRemoveEmpty(Split(pterms[j], "="));
				vector<string> pvals;
				if (pterm.size() == 2)
					pvals = TrimRemoveEmpty(Split(pterm[1], " "));
				else if (pterm.size() == 1);
				else {
					vector<string> ptermg = pterm;
					ptermg.erase(ptermg.begin());
					pvals = TrimRemoveEmpty(Split(Join(ptermg, "="), " "));
				}
				mterms.push_back(pair<string, vector<string> >(ToLower(pterm[0]), pvals));
			}
			r.push_back(pair<string, MapSV>(ToLower(title), MapSV(mterms)));
		}
		return r;
	}

	class ParamTerm {
	public:
		void *ptr;
		string name;
		string type;
		int length;
		ParamTerm(void *p, string t, string na, int len) {
			ptr = p; type = t; name = na; length = len;
		}
	};

	vector<int> ParseIntList(const string& val) {
		const vector<string>& vg = TrimRemoveEmpty(Split(val, " \t"));
		vector<int> vi;
		for (size_t i = 0; i < vg.size(); i++) {
			const vector<string>& vgg = TrimRemoveEmpty(Split(vg[i], "~"));
			if (vgg.size() == 1) vi.push_back(atoi(vg[i].c_str()));
			else if (vgg.size() == 2) {
				int a = atoi(vgg[0].c_str());
				int b = atoi(vgg[0].c_str());
				for (int j = a; j <= b; j++)
					vi.push_back(j);
			} else throw fitting::exception("Parse Error: Value error: \n" + vg[i]);
		}
		return vi;
	}

	bool ParseBool(const string& val) {
		if (val == "F" || val == "f" || val == "false" || val == "False" || val == "0")
			return false;
		else
			return true;
	}

	// 集合中每一个元素做普通变换
	template<typename T, typename S> vector<S> Map(const vector<T>& x, S(*tts)(const T&)) {
		vector<S> r(x.size());
		for (size_t i = 0; i < x.size(); i++) r[i] = tts(x[i]);
		return r;
	}

	// 浮点数转换为 Fortran 字符串
	string DToStrF(double x) {
		stringstream ss; ss.precision(15);
		ss << x; string oi = ss.str();
		if (oi.length() >= 16) {
			stringstream ssg;
			ssg.precision(18);
			ssg << x; oi = ssg.str();
		}
		if (oi.find('e') != oi.npos)
			oi[oi.find('e')] = 'd';
		else oi += "d0";
		return oi;
	}

	// 连接
	template<typename T> static T Join(vector<T> x, const T& y, const T& zero) {
		T r = zero;
		for (size_t i = 0; i < x.size() - 1; i++) r += x[i] + y;
		if (x.size() != 0) r += x[x.size() - 1];
		return r;
	}

}

namespace fitting {

	template<typename T> class static_constructable {
	private:
		struct helper {
			helper() { T::static_constructor(); }
		};
	protected:
		static_constructable() { static helper placeholder; }
	};

	void AppendTo(vector<string>& a, const vector<string>& b) {
		for (size_t j = 0; j < b.size(); j++) a.push_back(b[j]);
	}

	enum ExprType { Num = 0, Var = 1, Ope = 2, Fun = 3, LP = 4, RP = 5, Comma = 6 };

	class Expr : static_constructable<Expr> {
	private:
		double Value;
		string Name;
		ExprType ET;
		vector<Expr> Exprs;
		static map<string, int> Infixl;
		static map<string, double(*)(double)> SingleFuns;
		static map<string, Expr(*)(const Expr&)> DiffFuns;
		bool integer;
	public:
		string Str;
		int count;
		static const double MPI;
		static const double ME;
		static string FUNLP, FUNRP, IntFUNLP, IntFUNRP;
		static bool fortran;
		Expr();
		Expr(double val);
		Expr(double val, bool i);
		Expr(const string& name);
		Expr(const string& name, const Expr& l, const Expr& r);
		Expr(ExprType et, const string& name, const vector<Expr>& exprs);
		Expr(const string& name, const Expr& e);

		string show() const;
		vector<string> xshow() const;
		~Expr() {}
		static Expr parse(const string& str);
		Expr operator-() const;
		friend Expr operator+(const Expr& x, const Expr& y);
		friend Expr operator-(const Expr& x, const Expr& y);
		friend Expr operator*(const Expr& x, const Expr& y);
		friend Expr operator/(const Expr& x, const Expr& y);
		friend Expr operator^(const Expr& x, const Expr& y);
		friend Expr operator%(const string& a, const Expr& x);
		friend Expr operator%(const string& a, const vector<Expr>& x);
		friend vector<Expr> operator,(const Expr& x, const Expr& y);
		friend vector<Expr> operator,(const vector<Expr>& x, const Expr& y);
		friend bool operator==(const Expr& x, const Expr& y);
		friend bool operator<(const Expr& x, const Expr& y);
		double eval() const;
		double eval(const map<string, Expr>& vars) const;
		Expr subs(const map<Expr, Expr>& vars) const;
		Expr diff(const vector<string>& dxs, const map<string, Expr>& funs) const;
		Expr diff(const string& dx, const map<string, Expr>& funs) const;
		Expr diff(const vector<string>& dxs) const;
		Expr diff(const string& dx) const;
		Expr simplify() const;
		Expr powify() const;
		Expr expand() const;
		Expr sortterm() const;
		Expr merge() const;
		vector<string> getvars() const; // 返回公式中所有的变量名
		vector<pair<Expr, Expr> > coef(Expr x) const;
		static void static_constructor();
		double getvalue() const;
		Expr reduce_term(const string& name, int cnt);
		vector<pair<string, Expr> > reduce_terms(const string& name, int cnt);
		vector<Expr> decomma() const; // 返回逗号表达式中每一项
		int getcount() const; // 有多少项
	private:
		bool samefrom(const vector<Expr>& a, const vector<Expr>& b, size_t i, size_t j) const;
		string showp(const string& name, const Expr& x) const;
		string showpp(const string& name, const Expr& x) const;
		vector<string> xshowp(const string& name, const Expr& x) const;
		vector<string> xshowpp(const string& name, const Expr& x) const;
		static Expr ipexpr(const vector<string>& vs, const vector<ExprType>& vt, size_t& i, bool get);
		static Expr ipterm(const vector<string>& vs, const vector<ExprType>& vt, size_t& i, bool get);
		static Expr ippowe(const vector<string>& vs, const vector<ExprType>& vt, size_t& i, bool get);
		static Expr ipprim(const vector<string>& vs, const vector<ExprType>& vt, size_t& i, bool get);
	};

	class DiffFormula {
	public:
		static Expr dsin(const Expr& expr) { return "cos" % expr; }
		static Expr dcos(const Expr& expr) { return -("sin" % expr); }
		static Expr dtan(const Expr& expr) { return ("cos" % expr) ^ -2; }
		static Expr dsinh(const Expr& expr) { return "cosh" % expr; }
		static Expr dcosh(const Expr& expr) { return "sinh" % expr; }
		static Expr dtanh(const Expr& expr) { return ("cosh" % expr) ^ 2; }
		static Expr dcot(const Expr& expr) { return -("sin" % expr) ^ -2; }
		static Expr dasin(const Expr& expr) { return 1 / ("sqrt" % (1 - (expr ^ 2))); }
		static Expr dacos(const Expr& expr) { return -1 / ("sqrt" % (1 - (expr ^ 2))); }
		static Expr datan(const Expr& expr) { return 1 / (1 + expr ^ 2); }
		static Expr dexp(const Expr& expr) { return "exp" % expr; }
		static Expr dln(const Expr& expr) { return expr ^ -1; }
		static Expr dsqrt(const Expr& expr) { return 1 / (2 * ("sqrt" % expr)); }
		static Expr dminus(const Expr& expr) { return -1; }
		static Expr dplus(const Expr& expr) { return 1; }
	};

	string DoubleToString(double d) {
		stringstream ss;
		ss.precision(10);
		ss << d;
		return ss.str();
	}

	string DoubleToStringFor(double d) {
		stringstream ss;
		ss.precision(15);
		ss << d;
		string oi = ss.str();
		if (oi.length() >= 16) {
			stringstream ssv;
			ssv.precision(18);
			ssv << d;
			oi = ssv.str();
		}
		if (oi.find('e') != oi.npos)
			oi[oi.find('e')] = 'd';
		else oi += "d0";
		return oi;
	}

	double StringToDouble(const string& x) {
		return strtod(x.c_str(), NULL);
	}

	exception::exception(const string& x) { g = x; }

	map<string, int> Expr::Infixl;
	map<string, double(*)(double)> Expr::SingleFuns;
	map<string, Expr(*)(const Expr&)> Expr::DiffFuns;
	const double Expr::MPI = 3.14159265358979323846;
	const double Expr::ME = 2.71828182845904523536;
	string Expr::FUNLP = "(";
	string Expr::IntFUNLP = "(";
	string Expr::FUNRP = ")";
	string Expr::IntFUNRP = ")";
	bool Expr::fortran = false;

	double cot(double x) { return 1 / tan(x); }
	double pow2(double x) { return pow(x, 2); }
	double acos2(double x) { return (x < 1 && x >= -1) ? acos(x) : (x >= 1 ? 0 : Expr::MPI); }
	double sqrt2(double x) { return x > 0 ? sqrt(x) : 0; }

	void Expr::static_constructor() {
		Infixl["+"] = Infixl["-"] = 1;
		Infixl["*"] = Infixl["/"] = 2;
		Infixl["^"] = 3;

		SingleFuns["sin"] = &sin; SingleFuns["cos"] = &cos; SingleFuns["tan"] = &tan; SingleFuns["cot"] = &cot;
		SingleFuns["asin"] = &asin; SingleFuns["acos"] = &acos2; SingleFuns["atan"] = &atan;
		SingleFuns["sinh"] = &sinh; SingleFuns["cosh"] = &cosh; SingleFuns["tanh"] = &tanh;
		SingleFuns["abs"] = &abs; SingleFuns["sqrt"] = &sqrt2;
		SingleFuns["exp"] = &exp; SingleFuns["ln"] = &log; SingleFuns["pow"] = &pow2;

		DiffFuns["+"] = &DiffFormula::dplus; DiffFuns["-"] = &DiffFormula::dminus;
		DiffFuns["sin"] = &DiffFormula::dsin; DiffFuns["cos"] = &DiffFormula::dcos;
		DiffFuns["tan"] = &DiffFormula::dtan; DiffFuns["cot"] = &DiffFormula::dcot;
		DiffFuns["asin"] = &DiffFormula::dasin; DiffFuns["acos"] = &DiffFormula::dacos;
		DiffFuns["atan"] = &DiffFormula::datan; DiffFuns["tanh"] = &DiffFormula::dtanh;
		DiffFuns["sinh"] = &DiffFormula::dsinh; DiffFuns["cosh"] = &DiffFormula::dcosh;
		DiffFuns["sqrt"] = &DiffFormula::dsqrt; DiffFuns["exp"] = &DiffFormula::dexp;
		DiffFuns["ln"] = &DiffFormula::dln;
	}

	Expr Expr::ipexpr(const vector<string>& vs, const vector<ExprType>& vt, size_t& i, bool get) {
		vector<Expr> ve;
		ve.push_back(ipterm(vs, vt, i, get));
		for (; i < vs.size();) {
			if (vs[i] == "+") ve.push_back(ipterm(vs, vt, i, true));
			else if (vs[i] == "-") ve.push_back(-ipterm(vs, vt, i, true));
			else break;
		}
		return ve.size() > 1 ? Expr(Ope, "+", ve) : ve[0];
	}

	Expr Expr::ipterm(const vector<string>& vs, const vector<ExprType>& vt, size_t& i, bool get) {
		vector<Expr> ve;
		ve.push_back(ippowe(vs, vt, i, get));
		for (; i < vs.size();) {
			if (vs[i] == "*") ve.push_back(ippowe(vs, vt, i, true));
			else if (vs[i] == "/") ve.push_back(ippowe(vs, vt, i, true) ^ (-1));
			else break;
		}
		return ve.size() > 1 ? Expr(Ope, "*", ve) : ve[0];
	}

	Expr Expr::ippowe(const vector<string>& vs, const vector<ExprType>& vt, size_t& i, bool get) {
		vector<Expr> ve;
		ve.push_back(ipprim(vs, vt, i, get));
		for (; i < vs.size();) {
			if (vs[i] == "^") ve.push_back(ipprim(vs, vt, i, true));
			else break;
		}
		return ve.size() > 1 ? Expr(Ope, "^", ve) : ve[0];
	}

	Expr Expr::ipprim(const vector<string>& vs, const vector<ExprType>& vt, size_t& i, bool get) {
		if (get) i++;
		if (vt[i] == Num) { return Expr(StringToDouble(vs[i++])); } else if (vt[i] == Var) {
			string h = vs[i];
			i++;
			if (i < vt.size() && (vt[i] == Var || vt[i] == Num || vt[i] == LP)) {
				Expr t = ipprim(vs, vt, i, false);
				return t.ET == Comma ? Expr(Fun, h, t.Exprs) : Expr(h, t);
			} else return Expr(h);
		} else if (vs[i] == "+") return ipprim(vs, vt, i, true);
		else if (vs[i] == "-") return -ipprim(vs, vt, i, true);
		else if (vs[i] == "(") {
			vector<Expr> ve;
			ve.push_back(ipexpr(vs, vt, i, true));
			while (vt[i] == Comma)
				ve.push_back(ipexpr(vs, vt, i, true));
			if (vt[i] != RP)
				throw fitting::exception(") expected!");
			i++;
			return ve.size() == 1 ? ve[0] : Expr(Comma, ",", ve);
		} else
			throw fitting::exception("primary expected!");
	}

	Expr Expr::parse(const string& str) {
		vector<string> vs;
		vector<ExprType> vt;
		for (size_t i = 0; i < str.length(); i++) {
			char c = str[i];
			if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '\'' || c == '_') {
				size_t k = i;
				for (; k < str.length(); k++) {
					char d = str[k];
					if (!((d >= 'a' && d <= 'z') || (d >= 'A' && d <= 'Z') || (d >= '0' && d <= '9') || d == '\'' || d == '_'))
						break;
				}
				vt.push_back(Var);
				vs.push_back(str.substr(i, k - i));
				i = k - 1;
			} else if ((c >= '0' && c <= '9') || c == '.') {
				int has_e = -1;
				bool has_m = false;
				bool no_sgn = false;
				size_t k = i;
				for (; k < str.length(); k++) {
					char d = str[k];
					if (!((d >= '0' && d <= '9') || (d == '.' && has_e == -1) || ((d == 'e' || d == 'E' || d == 'd' || d == 'D') && has_e == -1)
						|| ((d == '+' || d == '-') && !has_m && has_e != -1 && !no_sgn)))
						break;
					if (d == 'e' || d == 'E' || d == 'd' || d == 'D') has_e = k;
					if (d == '+' || d == '-') has_m = true;
					if (d >= '0' && d <= '9' && has_e != -1 && has_e + 1 == k) no_sgn = true;
				}
				vt.push_back(Num);
				string subs = str.substr(i, k - i);
				if (has_e != -1) subs[has_e - i] = 'e';
				vs.push_back(subs);
				i = k - 1;
			} else if (c == ' ') continue;
			else if (c == '*' && (i + 1 < str.length() && str[i + 1] == '*')) {
				vs.push_back("^");
				vt.push_back(Ope); i++;
			} else if (c == '-' || c == '+' || c == '*' || c == '/' || c == '^') {
				vs.push_back(str.substr(i, 1));
				vt.push_back(Ope);
			} else if (c == '(' || c == ')') {
				vs.push_back(str.substr(i, 1));
				vt.push_back(c == '(' ? LP : RP);
			} else if (c == ',') {
				vs.push_back(str.substr(i, 1));
				vt.push_back(Comma);
			}
		}
		size_t ii = 0;
		return ipexpr(vs, vt, ii, false);
	}

	Expr::Expr() : ET(Num), Value(0) { Str = show(); count = getcount(); }
	Expr::Expr(double val) : ET(Num), Value(val), integer(false) { Str = show(); count = getcount(); }
	Expr::Expr(double val, bool i) : ET(Num), Value(val), integer(i) { Str = show(); count = getcount(); }
	Expr::Expr(const string& name) : ET(Var), Name(name) { Str = show(); count = getcount(); }
	Expr::Expr(const string& name, const Expr& l, const Expr& r) : ET(Ope), Name(name) {
		Exprs = vector<Expr>();
		Exprs.push_back(l); Exprs.push_back(r); Str = show(); count = getcount();
	}
	Expr::Expr(ExprType et, const string& name, const vector<Expr>& exprs) : ET(et), Name(name), Exprs(exprs) {
		Str = show(); count = getcount();
	}
	Expr::Expr(const string& name, const Expr& e) : ET(Fun), Name(name) {
		Exprs = vector<Expr>();
		Exprs.push_back(e); Str = show(); count = getcount();
	}

	Expr Expr::reduce_term(const string& name, int cnt) { // 通过中间变量减少项的数目
		int mi = 0; Expr r;
		for (size_t i = 1; i < Exprs.size(); i++)
			if (Exprs[i].count > Exprs[mi].count)
				mi = i;
		if (Exprs[mi].count < cnt) {
			r = Exprs[mi];
			Exprs[mi] = Expr(name);
		} else r = Exprs[mi].reduce_term(name, cnt);
		count = getcount();
		return r;
	}

	vector<pair<string, Expr> > Expr::reduce_terms(const string& name, int cnt) {
		vector<pair<string, Expr> > vpse;
		int i = 0;
		while (count >= cnt) {
			string nn = name + DoubleToString(i++);
			vpse.push_back(pair<string, Expr>(nn, reduce_term(nn, cnt)));
		}
		return vpse;
	}

	int Expr::getcount() const {
		if (ET == Num || ET == Var) return 1;
		else {
			int c = 0;
			for (size_t i = 0; i < Exprs.size(); i++)
				c += Exprs[i].count;
			return c;
		}
	}

	vector<string> Expr::xshowp(const string& name, const Expr& x) const {
		if (x.ET == Ope) {
			if (!((name == x.Name && (name == "*" || name == "+")) || Infixl[name] < Infixl[x.Name])) {
				vector<string> vs(x.xshow());
				vs.insert(vs.begin(), "("); vs.push_back(")");
				return vs;
			} else return x.xshow();
		} else if (x.ET == Fun && (x.Name == "+" || x.Name == "-")) {
			vector<string> vs(x.xshow());
			vs.insert(vs.begin(), "("); vs.push_back(")");
			return vs;
		} else if (x.ET == Num && x.Value < 0) {
			vector<string> vs(x.xshow());
			vs.insert(vs.begin(), "("); vs.push_back(")");
			return vs;
		} else return x.xshow();
	}

	vector<string> Expr::xshowpp(const string& name, const Expr& x) const {
		if (x.ET == Fun && (x.Name == "-" || x.Name == "+") && name == "+" && x.Exprs.size() == 1) {
			vector<string> vs(xshowp("-", x.Exprs[0]));
			vs.insert(vs.begin(), "-");
			if (!(x.Exprs[0].ET == Var || x.Exprs[0].ET == Num)) vs.insert(vs.begin() + 1, " ");
			return vs;
		} else  if (x.ET == Ope && x.Name == "^" && name == "*" && x.Exprs.size() == 2 && x.Exprs[1] == -1) {
			vector<string> vs(xshowp("/", x.Exprs[0]));
			vs.insert(vs.begin(), "/");
			if (!(x.Exprs[0].ET == Var || x.Exprs[0].ET == Num)) vs.insert(vs.begin() + 1, " ");
			return vs;
		} else {
			vector<string> vs(xshowp(name, x));
			vs.insert(vs.begin(), name == "^" ? "**" : name);
			if (!(x.ET == Var || x.ET == Num)) vs.insert(vs.begin() + 1, " ");
			return vs;
		}
	}

	vector<string> Expr::xshow() const {
		if (ET == Fun && (Name == "+" || Name == "-") && Exprs.size() == 1 &&
			((Exprs[0].ET == Num && Exprs[0].Value > 0) || Exprs[0].ET == Var)) {
			vector<string> vs(Exprs[0].xshow());
			vs.insert(vs.begin(), Name);
			return vs;
		} else if (ET == Fun) {
			bool sf = Expr::SingleFuns.find(Name) != Expr::SingleFuns.end();
			vector<string> vs;
			vs.push_back(Name);
			vs.push_back(sf ? Expr::IntFUNLP.c_str() : Expr::FUNLP.c_str());
			for (size_t i = 0; i < Exprs.size() - 1; i++) {
				AppendTo(vs, Exprs[i].xshow());
				vs.push_back(","); vs.push_back(" ");
			}
			AppendTo(vs, Exprs[Exprs.size() - 1].xshow());
			vs.push_back(sf ? Expr::IntFUNRP.c_str() : Expr::FUNRP.c_str());
			return vs;
		} else if (ET == Ope) {
			vector<string> vs;
			AppendTo(vs, xshowp(Name, Exprs[0]));
			if (!(Exprs[0].ET == Var || Exprs[0].ET == Num)) vs.push_back(" ");
			AppendTo(vs, xshowpp(Name, Exprs[1]));
			for (size_t k = 2; k < Exprs.size(); k++) {
				if (!(Exprs[k - 1].ET == Var || Exprs[k - 1].ET == Num)) vs.push_back(" ");
				AppendTo(vs, xshowpp(Name, Exprs[k]));
			}
			return vs;
		} else if (ET == Var) {
			vector<string> vs; vs.push_back(Name); return vs;
		} else {
			vector<string> vs; vs.push_back(integer ? DoubleToString(Value) :
				DoubleToStringFor(Value)); return vs;
		}
	}

	string Expr::showp(const string& name, const Expr& x) const {
		if (x.ET == Ope) {
			if (!((name == x.Name && (name == "*" || name == "+")) || Infixl[name] < Infixl[x.Name]))
				return "(" + x.show() + ")";
			else return x.show();
		} else if (x.ET == Fun && (x.Name == "+" || x.Name == "-"))
			return "(" + x.show() + ")";
		else return x.show();
	}

	string Expr::showpp(const string& name, const Expr& x) const {
		if (x.ET == Fun && (x.Name == "-" || x.Name == "+") && name == "+" && x.Exprs.size() == 1)
			return "-" + (x.Exprs[0].ET == Var || x.Exprs[0].ET == Num ? string("") : " ") + showp("-", x.Exprs[0]);
		else  if (x.ET == Ope && x.Name == "^" && name == "*" && x.Exprs.size() == 2 && x.Exprs[1] == -1)
			return "/" + (x.Exprs[0].ET == Var || x.Exprs[0].ET == Num ? string("") : " ") + showp("/", x.Exprs[0]);
		else return name + (x.ET == Var || x.ET == Num ? string("") : " ") + showp(name, x);
	}

	string Expr::show() const {
		if (ET == Fun && (Name == "+" || Name == "-") && Exprs.size() == 1 &&
			((Exprs[0].ET == Num && Exprs[0].Value > 0) || Exprs[0].ET == Var)) {
			return Name + Exprs[0].show();
		} else if (ET == Fun) {
			bool sf = Expr::SingleFuns.find(Name) != Expr::SingleFuns.end();
			string x = Name + (sf ? Expr::IntFUNLP.c_str() : Expr::FUNLP.c_str());
			for (size_t i = 0; i < Exprs.size() - 1; i++)
				x += Exprs[i].show() + ", ";
			return x + Exprs[Exprs.size() - 1].show() + (sf ? Expr::IntFUNRP.c_str() : Expr::FUNRP.c_str());
		} else if (ET == Ope) {
			string opn[] = { "+", "-", "*", "/", fortran ? "**" : "^" };
			string x = showp(Name, Exprs[0]) +
				(Exprs[0].ET == Var || Exprs[0].ET == Num ? string("") : " ") + showpp(Name, Exprs[1]);
			for (size_t k = 2; k < Exprs.size(); k++)
				x += (Exprs[k - 1].ET == Var || Exprs[k - 1].ET == Num ? string("") : " ") + showpp(Name, Exprs[k]);
			return x;
		} else if (ET == Var)
			return Name;
		else return fortran ? Parse::DToStrF(Value) : DoubleToString(Value);
	}

	Expr Expr::operator-() const {
		return Expr("-", *this);
	}

	Expr operator^(const Expr& x, const Expr& y) {
		return Expr("^", x, y);
	}

	Expr operator+(const Expr& x, const Expr& y) {
		return Expr("+", x, y);
	}

	Expr operator-(const Expr& x, const Expr& y) {
		return x + -y;
	}

	Expr operator*(const Expr& x, const Expr& y) {
		return Expr("*", x, y);
	}

	Expr operator/(const Expr& x, const Expr& y) {
		return x * (y ^ -1);
	}

	Expr operator%(const string& x, const Expr& y) {
		return Expr(x, y);
	}

	Expr operator%(const string& x, const vector<Expr>& y) {
		return Expr(Fun, x, y);
	}

	bool operator==(const Expr& x, const Expr& y) {
		if (x.ET != y.ET) return false;
		if (x.ET == Num) return x.Value == y.Value;
		else if (x.ET == Var) return x.Name == y.Name;
		else if (x.ET == Fun) return x.Name == y.Name && x.Exprs == y.Exprs;
		else if (x.ET == Ope) return x.Name == y.Name && x.Exprs == y.Exprs;
		else return true;
	}

	bool operator<(const Expr& x, const Expr& y) {
		if (x.ET != y.ET) return x.ET < y.ET;
		if (x.ET == Num) return x.Value < y.Value;
		else if (x.ET == Var) return x.Name < y.Name;
		else if (x.ET == Fun && x.Name != y.Name) return x.Name < y.Name;
		else if (x.ET == Fun) return x.Exprs < y.Exprs;
		else if (x.ET == Ope && x.Name != y.Name) return x.Name < y.Name;
		else if (x.ET == Ope) return x.Exprs < y.Exprs;
		else return true;
	}

	vector<Expr> operator,(const Expr& x, const Expr& y) {
		vector<Expr> r(2);
		r[0] = x; r[1] = y;
		return r;
	}

	vector<Expr> operator,(const vector<Expr>& x, const Expr& y) {
		vector<Expr> r = x;
		r.push_back(y);
		return r;
	}

	vector<Expr> Expr::decomma() const {
		if (ET != Comma) {
			vector<Expr> ve; ve.push_back(*this); return ve;
		} else return Exprs;
	}

	vector<string> Expr::getvars() const {
		if (ET == Var) {
			vector<string> k(1); k[0] = Name;
			return k;
		} else {
			vector<string> k;
			for (size_t i = 0; i < Exprs.size(); i++) {
				vector<string> r = Exprs[i].getvars();
				if (r.size() != 0)
					for (size_t j = 0; j < r.size(); j++) k.push_back(r[j]);
			}
			return k;
		}
	}

	Expr Expr::subs(const map<Expr, Expr>& vars) const {
		if (vars.find(*this) != vars.end())
			return vars.at(*this);
		else if (ET == Fun || ET == Ope) {
			vector<Expr> ve = Exprs;
			for (size_t i = 0; i < ve.size(); i++)
				ve[i] = ve[i].subs(vars);
			return Expr(ET, Name, ve);
		} else return *this;
	}

	double Expr::eval(const map<string, Expr>& vars) const {
		if (ET == Fun) {
			if (Name == "-") return -Exprs[0].eval(vars);
			else if (Name == "+") return Exprs[0].eval(vars);
			else if (SingleFuns.find(Name) != SingleFuns.end())
				return SingleFuns.at(Name)(Exprs[0].eval(vars));
			else throw fitting::exception(("function" + Name + "not implemented!").c_str());
		} else if (ET == Var) {
			if (Name == "pi") return MPI;
			else if (Name == "e") return ME;
			else if (vars.find(Name) != vars.end())
				return vars.at(Name).eval(vars);
			else throw fitting::exception(("variable" + Name + "undefined!").c_str());
		} else if (ET == Num) return Value;
		else if (ET == Ope) {
			if (Name == "^") {
				double k = Exprs[0].eval(vars);
				for (size_t i = 1; i < Exprs.size(); i++)
					k = pow(k, Exprs[i].eval(vars));
				return k;
			} else if (Name == "*") {
				double k = Exprs[0].eval(vars);
				for (size_t i = 1; i < Exprs.size(); i++)
					k *= Exprs[i].eval(vars);
				return k;
			} else if (Name == "+") {
				double k = Exprs[0].eval(vars);
				for (size_t i = 1; i < Exprs.size(); i++)
					k += Exprs[i].eval(vars);
				return k;
			} else throw fitting::exception(("operator" + Name + "not implemented!").c_str());
		} else throw fitting::exception(("type" + DoubleToString(ET) + "not support!").c_str());

	}

	double Expr::eval() const {
		map<string, Expr> mse;
		return eval(mse);
	}

	Expr Expr::powify() const {
		if (ET == Ope && Name == "^" && Exprs.size() == 2 && !(Exprs[1] == -1))
			return "pow" % (Exprs[0], Exprs[1]);
		else if (ET == Fun || ET == Ope) {
			vector<Expr> ve;
			for (size_t i = 0; i < Exprs.size(); i++)
				ve.push_back(Exprs[i].powify());
			return Expr(ET, Name, ve);
		} else return *this;
	}

	double Expr::getvalue() const { if (ET == Num) return Value; else return 0; }

	// 若要对某一中间变量保留导数形式，必须显式写出它对 dx 的依赖关系
	Expr Expr::diff(const string& dx, const map<string, Expr>& funs) const {
		if (dx == "") return *this;
		if (ET == Fun) {
			if (DiffFuns.find(Name) != DiffFuns.end())
				return DiffFuns.at(Name)(Exprs[0]) * Exprs[0].diff(dx, funs);
			else {
				vector<Expr> ve;
				const Expr *ex = this;
				while (ex->Name == "D") ex = &ex->Exprs[0];
				const vector<Expr>& vg = ex->Exprs;
				for (size_t i = 0; i < vg.size(); i++)
					ve.push_back(("D" % (*this, vg[i])) * vg[i].diff(dx, funs));
				return ve.size() > 1 ? Expr(Ope, "+", ve) : ve[0];
			}
		} else if (ET == Var) {
			if (Name == dx) return 1;
			else if (funs.find(Name) != funs.end())
				return funs.at(Name).diff(dx, funs);
			else return 0;
		} else if (ET == Num) return 0;
		else if (ET == Ope) {
			if (Name == "^") {
				vector<Expr> t = Exprs;
				Expr h = t[t.size() - 1]; t.pop_back();
				Expr tt = t.size() == 1 ? t[0] : Expr(Ope, "^", t);
				double c = h.eval();
				if (c == 0) return 0;
				else return c * (tt ^ (c - 1)) * tt.diff(dx, funs);
			} else if (Name == "*") {
				vector<Expr> t = Exprs;
				Expr h = t[t.size() - 1]; t.pop_back();
				Expr tt = t.size() == 1 ? t[0] : Expr(Ope, "*", t);
				return tt * h.diff(dx, funs) + tt.diff(dx, funs) * h;
			} else if (Name == "+") {
				vector<Expr> t(Exprs.size());
				for (size_t i = 0; i < t.size(); i++) t[i] = Exprs[i].diff(dx, funs);
				return Expr(Ope, "+", t);
			} else throw fitting::exception(("operator" + Name + "not implemented!").c_str());
		} else throw fitting::exception(("type" + DoubleToString(ET) + "not support!").c_str());
	}

	Expr Expr::diff(const string& dx) const {
		map<string, Expr> funs;
		return diff(dx, funs);
	}

	Expr Expr::diff(const vector<string>& dxs, const map<string, Expr>& funs) const {
		if (dxs.size() == 0) return *this;
		else {
			string h = dxs[0];
			vector<string> c(dxs);
			c.erase(c.begin());
			return diff(h, funs).diff(c, funs);
		}
	}

	Expr Expr::diff(const vector<string>& dxs) const {
		map<string, Expr> funs;
		return diff(dxs, funs);
	}

	Expr Expr::expand() const {
		if (ET == Ope) {
			if (Name == "*") {
				vector<Expr> ve = Exprs;
				for (size_t i = 0; i < ve.size(); i++) ve[i] = ve[i].expand();
				vector<vector<Expr> > ved(ve.size());
				bool need = false;
				for (size_t i = 0; i < ve.size(); i++) {
					if (ve[i].ET == Ope && ve[i].Name == "+")
						ved[i] = ve[i].Exprs, need = true;
					else ved[i].push_back(ve[i]);
				}
				if (need) {
					vector<Expr> vex;
					vector<int> vi(ved.size());
					while (true) {
						Expr v = 1; int j;
						for (size_t i = 0; i < ved.size(); i++)
							v = v * ved[i][vi[i]];
						vex.push_back(v);
						for (j = ved.size() - 1; j >= 0; j--)
							if (vi[j] < int(ved[j].size()) - 1) {
								vi[j]++;
								for (size_t k = j + 1; k < ved.size(); k++)
									vi[k] = 0;
								break;
							}
						if (j == -1) break;
					}
					return Expr(Ope, "+", vex).simplify();
				} else return Expr(Ope, "*", ve).simplify();
			} else if (Name == "^") {
				vector<Expr> ve = Exprs;
				for (size_t i = 0; i < ve.size(); i++) ve[i] = ve[i].expand();
				for (size_t i = 2; i < ve.size(); i++) ve[1] = ve[1] * ve[i];
				ve.resize(2);
				ve[1] = ve[1].expand().simplify();
				if (ve[1].ET == Num && ve[1].Value == floor(ve[1].Value) && ve[0].ET == Ope && ve[0].Name == "+") {
					int c = int(ve[1].Value);
					Expr t = 1;
					for (int i = 0; i < c; i++)
						t = t * ve[0];
					return t.expand().simplify();
				}
				return Expr(Ope, "^", ve).simplify();
			} else if (Name == "+") {
				vector<Expr> ve = Exprs;
				for (size_t i = 0; i < ve.size(); i++) ve[i] = ve[i].expand();
				return Expr(Ope, "+", ve).simplify();
			} else throw fitting::exception(("operator" + Name + "not implemented!").c_str());
		} else if (ET == Fun) {
			vector<Expr> ve = Exprs;
			for (size_t i = 0; i < ve.size(); i++) ve[i] = ve[i].expand();
			if (Name == "-" && ve.size() == 1)
				return Expr("*", -1, ve[0]);
			else if (Name == "+" && ve.size() == 1)
				return ve[0];
			else return Expr(Fun, Name, ve);
		} else return *this;
	}

	Expr Expr::sortterm() const {
		if (ET == Ope) {
			vector<Expr> ve = Exprs;
			for (size_t i = 0; i < ve.size(); i++) ve[i] = ve[i].sortterm();
			if (Name == "*" || Name == "^")
				sort(ve.begin(), ve.end());
			return Expr(Ope, Name, ve);
		} else if (ET == Fun) {
			vector<Expr> ve = Exprs;
			for (size_t i = 0; i < ve.size(); i++) ve[i] = ve[i].sortterm();
			return Expr(Fun, Name, ve);
		} else return *this;
	}

	bool Expr::samefrom(const vector<Expr>& a, const vector<Expr>& b, size_t i, size_t j) const {
		if (a.size() - i != b.size() - j) return false;
		for (size_t k = 0; k < a.size() - i; k++)
			if (!(a[i + k] == b[j + k])) return false;
		return true;
	}

	vector<pair<Expr, Expr> > Expr::coef(Expr x) const { // 指数, 系数
		vector<pair<Expr, Expr> > vpe;
		if (ET == Ope) {
			if (Name == "^") {
				if (Exprs.size() == 2 && Exprs[0] == x) {
					vpe.push_back(pair<Expr, Expr>(Exprs[1], 1));
					return vpe;
				}
			} else if (Name == "*") {
				pair<Expr, Expr> pee(0, 1);
				for (size_t i = 0; i < Exprs.size(); i++) {
					vector<pair<Expr, Expr> > c = Exprs[i].coef(x);
					if (c.size() == 1) {
						pee.first = pee.first + c[0].first;
						pee.second = pee.second * c[0].second;
					} else
						pee.second = pee.second * Exprs[i];
				}
				pee.first = pee.first.simplify();
				pee.second = pee.second.simplify();
				vpe.push_back(pee);
				return vpe;
			} else if (Name == "+") {
				for (size_t i = 0; i < Exprs.size(); i++) {
					vector<pair<Expr, Expr> > c = Exprs[i].coef(x);
					if (c.size() != 1) {
						c[0] = pair<Expr, Expr>(0, Exprs[i]);
						c.resize(1);
					}
					int h = -1;
					for (size_t k = 0; k < vpe.size(); k++) {
						if (vpe[k].first == c[0].first) {
							vpe[k].second = vpe[k].second + c[0].second;
							h = k; break;
						}
					}
					if (h == -1) {
						h = vpe.size();
						for (size_t k = 0; k < vpe.size(); k++) {
							if (c[0].first < vpe[k].first) {
								h = k; break;
							}
						}
						vpe.insert(vpe.begin() + h, c[0]);
					}
				}
				for (size_t i = 0; i < vpe.size(); i++)
					vpe[i].second = vpe[i].second.simplify();
				return vpe;
			}
		} else if (*this == x) {
			vpe.push_back(pair<Expr, Expr>(1, 1));
			return vpe;
		}
		vpe.push_back(pair<Expr, Expr>(0, *this));
		return vpe;
	}

	Expr Expr::merge() const {
		if (ET == Ope) {
			vector<Expr> ve = Exprs;
			for (size_t i = 0; i < ve.size(); i++)
				ve[i] = ve[i].sortterm().merge();
			if (Name == "*") {
				for (size_t i = 0; i < ve.size(); i++) {
					if (!(ve[i] == 1)) {
						for (size_t j = i + 1; j < ve.size(); j++) {
							if (ve[i] == ve[j])
								ve[i] = ve[i] ^ 2, ve[j] = 1;
							else if (ve[i].ET == Ope && ve[j].ET == Ope && ve[i].Name == "^" && ve[j].Name == "^"
								&& ve[i].Exprs.size() == 2 && ve[j].Exprs.size() == 2 && ve[i].Exprs[0] == ve[j].Exprs[0]) {
								ve[i] = ve[i].Exprs[0] ^ (ve[i].Exprs[1] + ve[j].Exprs[1]), ve[j] = 1;
							} else if (ve[i].ET == Ope && ve[i].Name == "^"
								&& ve[i].Exprs.size() == 2 && ve[i].Exprs[0] == ve[j]) {
								ve[i] = ve[i].Exprs[0] ^ (ve[i].Exprs[1] + 1), ve[j] = 1;
							} else if (ve[j].ET == Ope && ve[j].Name == "^"
								&& ve[j].Exprs.size() == 2 && ve[j].Exprs[0] == ve[i]) {
								ve[i] = ve[i] ^ (1 + ve[j].Exprs[1]), ve[j] = 1;
							}
						}
					}
				}
				return Expr(Ope, "*", ve).simplify();
			} else if (Name == "+") {
				vector<double> vd(ve.size());
				vector<vector<Expr> > vve(ve.size());
				for (size_t i = 0; i < ve.size(); i++) {
					vd[i] = 1;
					if (ve[i].ET == Ope && ve[i].Name == "*") {
						vve[i] = ve[i].Exprs;
						if (ve[i].Exprs[0].ET == Num)
							vd[i] = ve[i].Exprs[0].Value, vve[i].erase(vve[i].begin());
					} else vve[i].push_back(ve[i]);
				}
				for (size_t i = 0; i < ve.size(); i++) {
					if (vd[i] != 0) {
						for (size_t j = i + 1; j < ve.size(); j++) {
							if (samefrom(vve[i], vve[j], 0, 0))
								vd[i] += vd[j], vd[j] = 0;
						}
					}
				}
				for (size_t i = 0; i < ve.size(); i++)
					if (vve[i].size() == 1) ve[i] = vd[i] * vve[i][0];
					else ve[i] = vd[i] * Expr(Ope, "*", vve[i]);
					return Expr(Ope, "+", ve).simplify();
			} else return Expr(Ope, Name, ve).simplify();
		} else if (ET == Fun) {
			vector<Expr> ve = Exprs;
			for (size_t i = 0; i < ve.size(); i++) ve[i] = ve[i].sortterm().merge();
			return Expr(Fun, Name, ve).simplify();
		} else return *this;
	}

	Expr Expr::simplify() const {
		if (ET == Ope) {
			if (Name == "^") {
				vector<Expr> ve = Exprs;
				for (size_t i = 0; i < ve.size(); i++) ve[i] = ve[i].simplify();
				if (ve[0] == 0) return 0;
				else if (ve[0] == 1) return 1;
				for (size_t i = 1; i < ve.size(); i++) {
					if (ve[i] == 1) {
						ve.erase(ve.begin() + i);
						i--;
					} else if (ve[i] == 0) return 1;
				}
				if (ve.size() == 1) return ve[0];
				else return Expr(Ope, "^", ve);
			} else if (Name == "*") {
				vector<Expr> ve;
				for (size_t i = 0; i < Exprs.size(); i++) {
					Expr g = Exprs[i].simplify();
					if (g.ET == Ope && g.Name == "*")
						for (size_t j = 0; j < g.Exprs.size(); j++)
							ve.push_back(g.Exprs[j]);
					else ve.push_back(g);
				}
				double c = 1;
				for (size_t i = 0; i < ve.size(); i++)
					if (ve[i].ET == Num) {
						c *= ve[i].Value;
						ve.erase(ve.begin() + i);
						i--;
					}
				if (c == 0) return 0;
				if (ve.size() == 0) return c;
				else if (ve.size() == 1) return c != 1 ? c * ve[0] : ve[0];
				else return c != 1 ? c * Expr(Ope, "*", ve) : Expr(Ope, "*", ve);
			} else if (Name == "+") {
				vector<Expr> ve;
				for (size_t i = 0; i < Exprs.size(); i++) {
					Expr g = Exprs[i].simplify();
					if (g.ET == Ope && g.Name == "+")
						for (size_t j = 0; j < g.Exprs.size(); j++)
							ve.push_back(g.Exprs[j]);
					else ve.push_back(g);
				}
				double c = 0;
				for (size_t i = 0; i < ve.size(); i++)
					if (ve[i].ET == Num) {
						c += ve[i].Value;
						ve.erase(ve.begin() + i);
						i--;
					}
				if (ve.size() == 0) return c;
				else if (ve.size() == 1) return c != 0 ? c + ve[0] : ve[0];
				else return c != 0 ? c + Expr(Ope, "+", ve) : Expr(Ope, "+", ve);
			} else throw fitting::exception(("operator" + Name + "not implemented!").c_str());
		} else if (ET == Fun) {
			vector<Expr> ve = Exprs;
			for (size_t i = 0; i < ve.size(); i++) ve[i] = ve[i].simplify();
			return Expr(Fun, Name, ve);
		} else return *this;
	}
}

using namespace Parse;
using namespace fitting;
namespace Intpol {

	vector<ParamTerm> ParamSet;
	vector<string> AtomName;
	vector<double> AtomMass;
	vector<string> Coordinate;
	string MainFile;
	string OutputDir;
	double VCut, VMin, E1;
	vector<int> MainColumn;
	string Method;
	string ManyBodyMethod;
	bool ManyBody = false;
	string RABFile, RBCFile, RACFile;
	vector<int> RABColumn, RBCColumn, RACColumn;
	vector<double> MBVCut, MBVMin;

	class RKHS {
	public:
		string Type;
		double Exp;
		double Regularization;
		RKHS(string t, double e, double r) : Type(t), Exp(e), Regularization(r) {}
		RKHS() : Type(""), Exp(0), Regularization(0) {}
	};

	class Spline {
	public:
		string Boundary;
		bool Fast;
		Spline(string b, bool f) : Boundary(b), Fast(f) {}
		Spline() : Boundary(""), Fast(false) {}
	};

	class Task {
	public:
		int type; // 1: single point 2: opt 3: freq
		vector<double> point;
		int opt_type;
		double g_conv, st_conv, st_max;
		int n_step;
		Task(int t) : type(t) { opt_type = 1; g_conv = 1e-8; st_conv = 1e-8; st_max = 0.1; n_step = 100; }
	};

	vector<RKHS> vr(4);
	vector<Spline> vs(4);
	vector<Task> tasks;

	bool CompareThree(const vector<double>& a, const vector<double>& b) {
		if (a[2] != b[2]) return a[2] < b[2];
		else if (a[1] != b[1]) return a[1] < b[1];
		else if (a[0] != b[0]) return a[0] < b[0];
		else return a[3] < b[3];
	}

	bool CompareOne(const vector<double>& a, const vector<double>& b) {
		if (a[0] != b[0]) return a[0] < b[0];
		else return a[1] < b[1];
	}

	string WriteCode(const string& name, const vector<string>& vars, const vector<fitting::Expr>& exprs) {
		static size_t line_length = 100;
		stringstream ss;
		ss << "  function " + name + "(x)" << endl;
		ss << "    implicit none" << endl; 
		ss << "    real(8), parameter :: pi = " << Join(fitting::Expr(fitting::Expr::MPI).xshow(), "") << endl;
		if (exprs.size() / 3 == 1) ss << "    real(8) :: " + name + "(0:2)" << endl;
		else ss << "    real(8) :: " + name + "(0:" << exprs.size() / 3 - 1 << ", 0:2)" << endl;
		ss << "    real(8) :: x(0:2), " << Join(vars, ", ") << endl << endl;
		ss << "    ";
		for (size_t i = 0; i < vars.size(); i++) ss << vars[i] << " = x(" << i << ")" << (i == vars.size() - 1 ? "" : "; ");
		ss << endl;
		for (size_t i = 0; i < exprs.size(); i++) {
			int l = 0;
			if (exprs.size() / 3 == 1)
				ss << "    " << name << "(" << i << ") = ", l = 4 + name.length() + 5 + IntToString(i).length();
			else
				ss << "    " << name << "(" << i % (exprs.size() / 3) << ", " << i / (exprs.size() / 3) << ") = ",
				l = 4 + name.length() + 7 + IntToString(i % (exprs.size() / 3)).length() + IntToString(i / (exprs.size() / 3)).length();
			vector<string> vs = exprs[i].xshow();
			for (size_t j = 0; j < vs.size(); j++) {
				if (l + vs[j].length() + 1 < line_length) ss << vs[j], l += vs[j].length();
				else {
					ss << " &" << endl << "      " << vs[j];
					l = 6 + vs[j].length();
				}
			}
			ss << endl;
		}
		ss << "  end function " << name << endl << endl;
		return ss.str();
	}

	string WriteTrans(const string& name, const vector<string>& varx, const vector<fitting::Expr>& exprx, 
		const vector<string>& vari, const vector<fitting::Expr>& expri, const vector<string>& varp, const vector<fitting::Expr>& exprp) {
		static size_t line_length = 100;
		stringstream ss;
		ss << "  subroutine " + name + "(x, i, l)" << endl;
		ss << "    implicit none" << endl;
		for (size_t i = 0; i < exprp.size(); i++) {
			ss << "    real(8), parameter :: " << varp[i] << " = " << Join(exprp[i].xshow(), "") << endl;
		}
		ss << "    real(8) :: x(0:" << exprx.size() - 1 << "), i(0:" << expri.size() - 1 << ")" << endl;
		vector<string> varg = varx;
		for (size_t i = 0; i < vari.size(); i++) 
			if (find(varg.begin(), varg.end(), vari[i]) == varg.end()) varg.push_back(vari[i]);
		ss << "    real(8) :: " << Join(varg, ", ") << endl;
		ss << "    logical :: l" << endl;
		ss << endl;
		const vector<string> *vars[2] = { &varx, &vari };
		const vector<fitting::Expr> *exprs[2] = { &exprx, &expri };
		const string xi[2] = { "i", "x" };
		for (size_t u = 0; u < 2; u++) {
			if (u == 0) ss << "    if (.not. l) then" << endl;
			else ss << "    else" << endl;
			for (size_t i = 0; i < vars[u]->size(); i++)
				ss << "      " << (*vars[u])[i] << " = " << xi[u] << "(" << i << ")" << endl;
			for (size_t i = 0; i < exprs[u]->size(); i++) {
				int l = 0;
				ss << "      " << xi[1 - u] << "(" << i << ") = ", l = 6 + 1 + 5 + IntToString(i).length();
				vector<string> vs = (*exprs[u])[i].xshow();
				for (size_t j = 0; j < vs.size(); j++) {
					if (l + vs[j].length() + 1 < line_length) ss << vs[j], l += vs[j].length();
					else {
						ss << " &" << endl << "        " << vs[j];
						l = 8 + vs[j].length();
					}
				}
				ss << endl;
			}
		}
		ss << "    end if" << endl;
		ss << "  end subroutine " << name << endl << endl;
		return ss.str();
	}

	string WriteDeriv(const string& name, const vector<string>& varx, vector<fitting::Expr>& exprx,
		const vector<string>& varp, const vector<fitting::Expr>& exprp) {
		static size_t line_length = 100;
		static int maxcnt = 1700;
		stringstream ss;
		ss << "  subroutine " + name + "(x, e)" << endl;
		ss << "    implicit none" << endl;
		for (size_t i = 0; i < exprp.size(); i++) {
			ss << "    real(8), parameter :: " << varp[i] << " = " << Join(exprp[i].xshow(), "") << endl;
		}
		ss << "    real(8) :: x(0:" << varx.size() - 1 << "), e(0:" << exprx.size() / 3 - 1 << ",0:2)" << endl;
		ss << "    real(8) :: " << Join(varx, ", ") << endl;
		vector<vector<pair<string, Expr> > > vvpse(exprx.size());
		vector<string> vecv;
		for (size_t i = 0; i < exprx.size(); i++) {
			vvpse[i] = exprx[i].reduce_terms("tmp_" + IntToString(i) + "_", maxcnt);
			for (size_t j = 0; j < vvpse[i].size(); j++) vecv.push_back(vvpse[i][j].first);
		}
		if (vecv.size() != 0) {
			ss << "    real(8) :: ";
			int l = 0;
			for (size_t i = 0; i < vecv.size(); i++) {
				ss << vecv[i]; l += vecv[i].length();
				if (i != vecv.size() - 1) ss << ", "; l += 2;
				if (l + 6 >= line_length) {
					ss << " &" << endl << "      ";
					l = 6;
				}
			}
			ss << endl;
		}
		ss << endl;
		for (size_t i = 0; i < varx.size(); i++)
			ss << "    " << varx[i] << " = x(" << i << ")" << endl;
		for (size_t i = 0; i < exprx.size(); i++) {
			for (size_t j = 0; j < vvpse[i].size(); j++) {
				int k = 0;
				ss << "    " << vvpse[i][j].first << " = ", k = 4 + 3 + vvpse[i][j].first.length();
				vector<string> vsp = vvpse[i][j].second.xshow();
				for (size_t m = 0; m < vsp.size(); m++) {
					if (k + vsp[m].length() + 1 < line_length) ss << vsp[m], k += vsp[m].length();
					else ss << " &" << endl << "      " << vsp[m], k = 6 + vsp[m].length();
				}
				ss << endl;
			}
			int l = 0;
			ss << "    e(" << i % (exprx.size() / 3) << ", " << i / (exprx.size() / 3) << ") = ",
				l = 4 + 1 + 7 + IntToString(i % (exprx.size() / 3)).length() + IntToString(i / (exprx.size() / 3)).length();
			vector<string> vs = exprx[i].xshow();
			for (size_t j = 0; j < vs.size(); j++) {
				if (l + vs[j].length() + 1 < line_length) ss << vs[j], l += vs[j].length();
				else {
					ss << " &" << endl << "      " << vs[j];
					l = 6 + vs[j].length();
				}
			}
			ss << endl;
			if (vvpse[i].size() != 0) ss << endl;
		}
		ss << "  end subroutine " << name << endl << endl;
		return ss.str();
	}

	void Run(const vector<pair<string, MapSV> >& ip) {

		ParamSet.push_back(ParamTerm(&AtomName, "stringlist", "atomname", 3));
		ParamSet.push_back(ParamTerm(&AtomMass, "doublelist", "atommass", 3));
		ParamSet.push_back(ParamTerm(&Coordinate, "stringlist", "coordinate", 3));

		ParamSet.push_back(ParamTerm(&MainFile, "string", "mainfile", 0));
		ParamSet.push_back(ParamTerm(&MainColumn, "intlist", "maincolumn", 4));
		ParamSet.push_back(ParamTerm(&OutputDir, "string", "outputdir", 0));
		ParamSet.push_back(ParamTerm(&VCut, "double", "vcut", 0));
		ParamSet.push_back(ParamTerm(&VMin, "double", "vmin", 0));
		ParamSet.push_back(ParamTerm(&Method, "string", "method", 0));
		ParamSet.push_back(ParamTerm(&ManyBody, "bool", "manybody", 0));
		ParamSet.push_back(ParamTerm(&ManyBodyMethod, "string", "manybody::method", 0));
		ParamSet.push_back(ParamTerm(&E1, "double", "manybody::e1", 0));
		ParamSet.push_back(ParamTerm(&RABFile, "string", "manybody::rabfile", 0));
		ParamSet.push_back(ParamTerm(&RBCFile, "string", "manybody::rbcfile", 0));
		ParamSet.push_back(ParamTerm(&RACFile, "string", "manybody::racfile", 0));
		ParamSet.push_back(ParamTerm(&RABColumn, "intlist", "manybody::rabcolumn", 2));
		ParamSet.push_back(ParamTerm(&RBCColumn, "intlist", "manybody::rbccolumn", 2));
		ParamSet.push_back(ParamTerm(&RACColumn, "intlist", "manybody::raccolumn", 2));
		ParamSet.push_back(ParamTerm(&MBVCut, "doublelist", "manybody::vcut", 3));
		ParamSet.push_back(ParamTerm(&MBVMin, "doublelist", "manybody::vmin", 3));

		bool isfreq = false, isopt = false, ispoint = false;

		for (size_t k = 0; k < ip.size(); k++) {
			if (ip[k].first == "parameter" || ip[k].first == "manybody") {
				vector<pair<string, vector<string> > > pv = ip[k].second.mapsv;
				for (size_t i = 0; i < pv.size(); i++) {
					string title = pv[i].first;
					if (ip[k].first == "manybody") title = "manybody::" + title;
					bool handled = false;
					if (title == "frequencies" || title == "freq") { 
						if (!ispoint)
							throw fitting::exception("You need to set a point by 'point = ...;' before calculating frequencies!");
						tasks.push_back(Task(3)); 
						isfreq = true;
						handled = true;
					} else if (title == "point") {
						Task ts(1); 
						if (pv[i].second.size() == 3) {
							vector<double> vd = Map(pv[i].second, Parse::StringToDouble);
							ts.point = vd;
							ispoint = true;
						} else ts.point = vector<double>();
						tasks.push_back(ts);
						handled = true;
					}
					for (size_t j = 0; j < ParamSet.size(); j++)
						if (title == ParamSet[j].name) {
							if (ParamSet[j].type == "double")
								pv[i].second.size() == 1 ? *(double*)ParamSet[j].ptr = Parse::StringToDouble(pv[i].second[0])
								: throw fitting::exception("Need one double: " + title + ", size = " + IntToString(pv[i].second.size()));
							else if (ParamSet[j].type == "string")
								pv[i].second.size() == 1 ? *(string*)ParamSet[j].ptr = pv[i].second[0]
								: throw fitting::exception(("Need one string: " + title + ", size = " + IntToString(pv[i].second.size())));
							else if (ParamSet[j].type == "bool")
								pv[i].second.size() == 1 ? *(bool*)ParamSet[j].ptr = ParseBool(pv[i].second[0])
								: throw fitting::exception(("Need one bool: " + title + ", size = " + IntToString(pv[i].second.size())));
							else if (ParamSet[j].type == "intlist")
								pv[i].second.size() == ParamSet[j].length ? *(vector<int>*)ParamSet[j].ptr = Map(pv[i].second, StringToInt)
								: throw fitting::exception(("Need " + IntToString(ParamSet[j].length) + " integers : " + title + ", size = " +
									IntToString(pv[i].second.size())));
							else if (ParamSet[j].type == "doublelist")
								pv[i].second.size() == ParamSet[j].length ? *(vector<double>*)ParamSet[j].ptr = Map(pv[i].second, Parse::StringToDouble)
								: throw fitting::exception(("Need " + IntToString(ParamSet[j].length) + " doubles : " + title + ", size = " +
									IntToString(pv[i].second.size())));
							else if (ParamSet[j].type == "stringlist")
								pv[i].second.size() == ParamSet[j].length ? *(vector<string>*)ParamSet[j].ptr = pv[i].second
								: throw fitting::exception(("Need " + IntToString(ParamSet[j].length) + " strings : " + title + ", size = " +
									IntToString(pv[i].second.size())));
							else
								throw fitting::exception(("Parse Error: Bad parameter type: \n" + ParamSet[j].type));
							handled = true;
							break;
						}
					if (!handled) throw fitting::exception(("Parse Error: Bad parameter name: \n" + title));
				}
			} else if (ip[k].first == "opt" || ip[k].first == "optimization") {
				if (!ispoint)
					throw fitting::exception("You need to set a initial point by 'point = ...;' before optimization!");
				Task t(2);
				vector<pair<string, vector<string> > > pv = ip[k].second.mapsv;
				for (size_t i = 0; i < pv.size(); i++) {
					string title = pv[i].first;
					if (pv[i].second.size() != 1) throw fitting::exception(("Need 1 value : " + title + ", size = " +
						IntToString(pv[i].second.size())));
					string value = pv[i].second[0];
					if (title == "type") {
						if (value == "minimum" || value == "transition")
							t.opt_type = (value == "minimum" ? 1 : 2);
						else throw fitting::exception(("not support : " + value + " for opt::" + title));
					} else if (title == "iter") t.n_step = StringToInt(value);
					else if (title == "steplength") t.st_max = Parse::StringToDouble(value);
					else if (title == "stepconv") t.st_conv = Parse::StringToDouble(value);
					else if (title == "forceconv") t.g_conv = Parse::StringToDouble(value);
					else throw fitting::exception(("not support parameter : " + title + " for opt"));
				}
				tasks.push_back(t);
				isopt = true;
			} else if (ip[k].first == "rkhs") {
				int dim = -1; RKHS r;
				vector<pair<string, vector<string> > > pv = ip[k].second.mapsv;
				for (size_t i = 0; i < pv.size(); i++) {
					string title = pv[i].first;
					if (pv[i].second.size() != 1) throw fitting::exception(("Need 1 value : " + title + ", size = " +
						IntToString(pv[i].second.size())));
					string value = pv[i].second[0];
					if (title == "dim") dim = StringToInt(value);
					else if (title == "type") {
						if (value == "distancelike" || value == "anglelike")
							r.Type = value;
						else throw fitting::exception(("not support : " + value + " for RKHS::" + title));
					} else if (title == "exp") r.Exp = Parse::StringToDouble(value);
					else if (title == "regularization") r.Regularization = Parse::StringToDouble(value);
					else throw fitting::exception(("not support parameter : " + title + " for RKHS"));
				}
				if (dim < 0 || dim > 3) throw fitting::exception(("dim error : " + IntToString(dim) + " for RKHS"));
				if (r.Type.length() == 0) throw fitting::exception(("type error : " + r.Type + " for RKHS"));
				vr[dim] = r;
			} else if (ip[k].first == "spline") {
				int dim = -1; Spline s;
				vector<pair<string, vector<string> > > pv = ip[k].second.mapsv;
				for (size_t i = 0; i < pv.size(); i++) {
					string title = pv[i].first;
					if (pv[i].second.size() != 1) throw fitting::exception(("Need 1 value : " + title + ", size = " +
						IntToString(pv[i].second.size())));
					string value = pv[i].second[0];
					if (title == "dim") dim = StringToInt(value);
					else if (title == "boundary") {
						if (value == "natural" || value == "clamp")
							s.Boundary = value;
						else throw fitting::exception(("not support : " + value + " for Spline::" + title));
					} else if (title == "fast") s.Fast = ParseBool(value);
					else throw fitting::exception(("not support parameter : " + title + " for Spline"));
				}
				if (dim < 0 || dim > 3) throw fitting::exception(("dim error : " + IntToString(dim) + " for Spline"));
				if (s.Boundary.length() == 0) throw fitting::exception(("boundary error : " + s.Boundary + " for Spline"));
				if (s.Fast) throw fitting::exception("Fast Spline has not been implemented in this code!");
				vs[dim] = s;
			} else throw fitting::exception(("Parse Error: Bad group name: \n" + ip[k].first));
		}

		string main_raw = ReadFileContent(MainFile);
		const vector<string>& main_lines = TrimRemoveEmpty(Split(main_raw, "\r\n"));
		vector<vector<double> > main_rx;
		for (size_t i = 0; i < main_lines.size(); i++) {
			const vector<string>& line_raw = TrimRemoveEmpty(Split(main_lines[i], " \t"));
			if (line_raw.size() == 0) continue;
			vector<double> vd;
			for (size_t j = 0; j < MainColumn.size(); j++)
				vd.push_back(Parse::StringToDouble(line_raw[MainColumn[j] - 1]));
			bool all_zero = true;
			for (size_t j = 0; j < vd.size(); j++) if (vd[j] != 0) all_zero = false;
			if (all_zero) continue;
			vd[3] = vd[3] - VMin;
			main_rx.push_back(vd);
		}

		sort(main_rx.begin(), main_rx.end(), CompareThree);
		set<double> vi, vj, vk;
		set<pair<double, double> > vij, vjk;
		map<double, pair<set<double>, set<double> > > vivjvk;
		map<double, set<pair<double, double> > > vijvk;
		map<pair<double, double>, set<double> > vivjk;

		for (size_t i = 0; i < main_rx.size(); i++) {
			vi.insert(main_rx[i][0]);
			vj.insert(main_rx[i][1]);
			vk.insert(main_rx[i][2]);
			vij.insert(pair<double, double>(main_rx[i][0], main_rx[i][1]));
			vjk.insert(pair<double, double>(main_rx[i][1], main_rx[i][2]));
		}

		vector<double> vkx;

		for (set<double>::const_iterator i = vk.begin(); i != vk.end(); ++i) {
			vivjvk[*i] = pair<set<double>, set<double> >();
			vijvk[*i] = set<pair<double, double> >();
			vkx.push_back(*i);
		}

		for (set<pair<double, double> >::const_iterator i = vjk.begin(); i != vjk.end(); ++i) {
			vivjk[*i] = set<double>();
		}

		sort(vkx.begin(), vkx.end());

		for (size_t i = 0; i < main_rx.size(); i++) {
			vivjvk[main_rx[i][2]].first.insert(main_rx[i][0]);
			vivjvk[main_rx[i][2]].second.insert(main_rx[i][1]);
			vijvk[main_rx[i][2]].insert(pair<double, double>(main_rx[i][0], main_rx[i][1]));
			vivjk[pair<double, double>(main_rx[i][1], main_rx[i][2])].insert(main_rx[i][0]);
		}

		size_t uimm = vi.size(), ujmm = vj.size(), uk = vk.size();
		size_t uijmm = vij.size(), ujk = vjk.size();
		size_t uijmbk = 0, uimbk = 0, ujmbk = 0, uimbjk = 0;

		for (set<double>::const_iterator i = vk.begin(); i != vk.end(); ++i) {
			if (vijvk[*i].size() > uijmbk) uijmbk = vijvk[*i].size();
			if (vivjvk[*i].first.size() > uimbk) uimbk = vivjvk[*i].first.size();
			if (vivjvk[*i].second.size() > ujmbk) ujmbk = vivjvk[*i].second.size();
		}

		for (set<pair<double, double> >::const_iterator i = vjk.begin(); i != vjk.end(); ++i)
			if (vivjk[*i].size() > uimbjk) uimbjk = vivjk[*i].size();

		string rm; int step = 0;
		vector<string> mm(4);
		if (Method == "R3") rm = "3", step = 1, mm[1] = mm[2] = mm[3] = "R";
		else if (Method == "R2R") rm = "21", step = 2, mm[1] = mm[2] = mm[3] = "R";
		else if (Method == "R2S") rm = "21", step = 2, mm[1] = mm[2] = "R", mm[3] = "S";
		else if (Method == "RR2") rm = "12", step = 2, mm[1] = mm[2] = mm[3] = "R";
		else if (Method == "SR2") rm = "12", step = 2, mm[1] = "S", mm[2] = mm[3] = "R";
		else {
			if (Method.size() != 3) throw fitting::exception(("method value length invalid: " + Method));
			rm = "111", step = 3;
			for (size_t i = 0; i < 3; i++)
				if (Method[i] == 'R') mm[i + 1] = "R";
				else if (Method[i] == 'S') mm[i + 1] = "S";
				else throw fitting::exception(("method value invalid: " + Method));
		}

		size_t mbmm = 0;
		vector<vector<vector<double> > > mb_rx;

		if (ManyBody) {
			if (ManyBodyMethod == "R" || ManyBodyMethod == "S") mm[0] = ManyBodyMethod;
			else throw fitting::exception(("manybody method value invalid: " + ManyBodyMethod));

			string mbf[3] = { RABFile, RBCFile, RACFile };
			vector<int> mbc[3] = { RABColumn, RBCColumn, RACColumn };

			for (size_t k = 0; k < 3; k++) {
				string mb_raw = ReadFileContent(mbf[k]);
				const vector<string>& mb_lines = TrimRemoveEmpty(Split(mb_raw, "\r\n"));
				if (mb_lines.size() == 0) continue;
				vector<vector<double> > mb_rxx;
				for (size_t i = 0; i < mb_lines.size(); i++) {
					const vector<string>& line_raw = TrimRemoveEmpty(Split(mb_lines[i], " \t"));
					vector<double> vd;
					for (size_t j = 0; j < mbc[k].size(); j++)
						vd.push_back(Parse::StringToDouble(line_raw[mbc[k][j] - 1]));
					vd[1] = vd[1] - MBVMin[k];
					mb_rxx.push_back(vd);
				}
				sort(mb_rxx.begin(), mb_rxx.end(), CompareOne);
				if (mb_rxx.size() > mbmm) mbmm = mb_rxx.size();
				mb_rx.push_back(mb_rxx);
			}
		}

		vector<double> exps(4), regs(4);
		bool hasrkhs = false, hasspline = false, hd = false, ha = false, hc = false, hn = false;

		for (size_t i = 0; i < 4; i++) {
			if (mm[i] == "R") {
				hasrkhs = true;
				if (vr[i].Type == "distancelike") mm[i] = "D", hd = true;
				else mm[i] = "A", ha = true;
				exps[i] = vr[i].Exp;
				regs[i] = vr[i].Regularization;
			} else {
				hasspline = true;
				if (vs[i].Boundary == "clamp") mm[i] = "C", hc = true;
				else mm[i] = "N", hn = true;
			}
		}

		if (AtomName.size() != 3) throw fitting::exception("must set AtomName!");

		vector<pair<string, string> > defines;
		string sys = ToLower(AtomName[0]) + ToLower(AtomName[1]) + ToLower(AtomName[2]);
		string spaf = sys + "-pes-param.txt", smbf = sys + "-pes-manybody.txt";
		string smaf = sys + "-pes-main.txt", sprf = sys + "-pes-prep.txt";
		string sabf = sys + "-pes-mbrab.txt", sacf = sys + "-pes-mbrac.txt", sbcf = sys + "-pes-mbrbc.txt";
		string smbxf[3] = { sabf, sbcf, sacf };

		defines.push_back(pair<string, string>("READMAIN" + rm, ""));
		defines.push_back(pair<string, string>("VCUT", DToStrF(VCut - VMin)));
		defines.push_back(pair<string, string>("PARAMFILE", "'" + spaf + "'"));
		defines.push_back(pair<string, string>("MAINFILE", "'" + smaf + "'"));
		defines.push_back(pair<string, string>("PREPFILE", "'" + sprf + "'"));
		if (ManyBody) {
			defines.push_back(pair<string, string>("MBVCUT", "(/ " + DToStrF(MBVCut[0] - MBVMin[0]) + ", " +
				DToStrF(MBVCut[1] - MBVMin[1]) + ", " + DToStrF(MBVCut[2] - MBVMin[2]) + " /)"));
			defines.push_back(pair<string, string>("MANYBODYFILE", "'" + smbf + "'"));
			defines.push_back(pair<string, string>("RABFILE", "'" + sabf + "'"));
			defines.push_back(pair<string, string>("RBCFILE", "'" + sbcf + "'"));
			defines.push_back(pair<string, string>("RACFILE", "'" + sacf + "'"));
			defines.push_back(pair<string, string>("E1", DToStrF(E1 - VMin)));
		}
		if (ManyBody) defines.push_back(pair<string, string>("MANYBODY", ""));
		if (hasrkhs) defines.push_back(pair<string, string>("RKHS", ""));
		if (hasspline) defines.push_back(pair<string, string>("SPLINE", ""));
		if (hd) defines.push_back(pair<string, string>("DISTANCELIKE", ""));
		if (ha) defines.push_back(pair<string, string>("ANGLELIKE", ""));
		if (hc) defines.push_back(pair<string, string>("CLAMP", ""));
		if (hn) defines.push_back(pair<string, string>("NATURAL", ""));
		for (size_t i = 0; i < 4; i++) {
			if (mm[i] == "D" || mm[i] == "A") {
				defines.push_back(pair<string, string>("MAIN" + IntToString(i) + "R", mm[i] == "A" ? "1" : "0"));
				defines.push_back(pair<string, string>("MAIN" + IntToString(i) + mm[i], ""));
				defines.push_back(pair<string, string>("MAIN" + IntToString(i) + "RK(x1, x2)",
					vr[i].Type + "_kernel(x1, x2" + (mm[i] == "A" ? "" : ", " + IntToString(i)) + ")"));
				defines.push_back(pair<string, string>("MAIN" + IntToString(i) + "RP(gsum, n, r, x)",
					vr[i].Type + "_presum(gsum, n, r, x" + (mm[i] == "A" ? "" : ", " + IntToString(i)) + ")"));
				defines.push_back(pair<string, string>("MAIN" + IntToString(i) + "RE(gsum, n, r, x)",
					vr[i].Type + "_eval(gsum, n, r, x" + (mm[i] == "A" ? "" : ", " + IntToString(i)) + ")"));
				defines.push_back(pair<string, string>("MAIN" + IntToString(i) + "RED(gsum, n, r, x)",
					vr[i].Type + "_evald(gsum, n, r, x" + (mm[i] == "A" ? "" : ", " + IntToString(i)) + ")"));
				defines.push_back(pair<string, string>("MAIN" + IntToString(i) + "REDD(gsum, n, r, x)",
					vr[i].Type + "_evaldd(gsum, n, r, x" + (mm[i] == "A" ? "" : ", " + IntToString(i)) + ")"));
			} else {
				defines.push_back(pair<string, string>("MAIN" + IntToString(i) + "S", ""));
				defines.push_back(pair<string, string>("MAIN" + IntToString(i) + mm[i], ""));
				defines.push_back(pair<string, string>("MAIN" + IntToString(i) + "SS", vs[i].Boundary + "_second"));
			}
		}
		defines.push_back(pair<string, string>("DERIVATIVE1", ""));
		defines.push_back(pair<string, string>("DERIVATIVE2", ""));
		if (ManyBody) {
			defines.push_back(pair<string, string>("UMM", IntToString(mbmm)));
		}

		if (rm == "3") {
			defines.push_back(pair<string, string>("UIJK", IntToString(main_rx.size())));
			defines.push_back(pair<string, string>("UIM", IntToString(uimm)));
			defines.push_back(pair<string, string>("UJM", IntToString(ujmm)));
			defines.push_back(pair<string, string>("UKM", IntToString(uk)));
		} else if (rm == "21") {
			defines.push_back(pair<string, string>("UK", IntToString(uk)));
			defines.push_back(pair<string, string>("UIJM", IntToString(uijmbk)));
			vector<string> va, vb;
			for (size_t i = 0; i < vkx.size(); i++) {
				va.push_back(IntToString(vivjvk[vkx[i]].first.size()));
				vb.push_back(IntToString(vivjvk[vkx[i]].second.size()));
			}
			string suim = "(/ " + Join(va, ", ") + " /)";
			string sujm = "(/ " + Join(vb, ", ") + " /)";

			defines.push_back(pair<string, string>("UIM", suim));
			defines.push_back(pair<string, string>("UJM", sujm));
		} else if (rm == "12") {
			defines.push_back(pair<string, string>("UJK", IntToString(ujk)));
			defines.push_back(pair<string, string>("UIM", IntToString(uimbjk)));
			defines.push_back(pair<string, string>("UJM", IntToString(ujmm)));
			defines.push_back(pair<string, string>("UKM", IntToString(uk)));
		} else if (rm == "111") {
			defines.push_back(pair<string, string>("UK", IntToString(uk)));
			defines.push_back(pair<string, string>("UJM", IntToString(ujmbk)));
			defines.push_back(pair<string, string>("UIM", IntToString(uimbjk)));
		}
		defines.push_back(pair<string, string>("AMA", DToStrF(AtomMass[0])));
		defines.push_back(pair<string, string>("AMB", DToStrF(AtomMass[1])));
		defines.push_back(pair<string, string>("AMC", DToStrF(AtomMass[2])));
		defines.push_back(pair<string, string>("ANA", "'" + AtomName[0] + "'"));
		defines.push_back(pair<string, string>("ANB", "'" + AtomName[1] + "'"));
		defines.push_back(pair<string, string>("ANC", "'" + AtomName[2] + "'"));
		defines.push_back(pair<string, string>("ANAX", "'" + AtomName[0] + "X'"));
		defines.push_back(pair<string, string>("ANBX", "'" + AtomName[1] + "X'"));
		defines.push_back(pair<string, string>("ANCX", "'" + AtomName[2] + "X'"));
		defines.push_back(pair<string, string>("ANAY", "'" + AtomName[0] + "Y'"));
		defines.push_back(pair<string, string>("ANBY", "'" + AtomName[1] + "Y'"));
		defines.push_back(pair<string, string>("ANCY", "'" + AtomName[2] + "Y'"));
		defines.push_back(pair<string, string>("ANAZ", "'" + AtomName[0] + "Z'"));
		defines.push_back(pair<string, string>("ANBZ", "'" + AtomName[1] + "Z'"));
		defines.push_back(pair<string, string>("ANCZ", "'" + AtomName[2] + "Z'"));

		if (ispoint) {
			string coordex[3] = { "", "", "" };
			for (size_t k = 0; k < 3; k++) {
				Coordinate[k] = ToLower(Coordinate[k]);
				for (size_t l = 0; l < Coordinate[k].length(); l++)
					if (Coordinate[k][l] == 'a')
						coordex[k] += AtomName[0];
					else if (Coordinate[k][l] == 'b')
						coordex[k] += AtomName[1];
					else if (Coordinate[k][l] == 'c')
						coordex[k] += AtomName[2];
					else coordex[k] += Coordinate[k][l];
			}
			defines.push_back(pair<string, string>("COORDA", "'" + coordex[0] + "'"));
			defines.push_back(pair<string, string>("COORDB", "'" + coordex[1] + "'"));
			defines.push_back(pair<string, string>("COORDC", "'" + coordex[2] + "'"));

			string vsr[3] = { "rab", "rbc", "rac" };
			string coordey[3] = { "", "", "" };
			for (size_t k = 0; k < 3; k++) {
				for (size_t l = 0; l < vsr[k].length(); l++)
					if (vsr[k][l] == 'a')
						coordey[k] += AtomName[0];
					else if (vsr[k][l] == 'b')
						coordey[k] += AtomName[1];
					else if (vsr[k][l] == 'c')
						coordey[k] += AtomName[2];
					else coordey[k] += vsr[k][l];
			}
			defines.push_back(pair<string, string>("COORDIA", "'" + coordey[0] + "'"));
			defines.push_back(pair<string, string>("COORDIB", "'" + coordey[1] + "'"));
			defines.push_back(pair<string, string>("COORDIC", "'" + coordey[2] + "'"));
		}

		if (ispoint) {
			defines.push_back(pair<string, string>("#ifdef RUN", ""));
			if (ispoint) defines.push_back(pair<string, string>("POINT", ""));
			if (isopt) defines.push_back(pair<string, string>("OPTM", ""));
			if (isfreq) defines.push_back(pair<string, string>("FREQ", ""));
			defines.push_back(pair<string, string>("#endif", ""));
		}

		stringstream mbcode, pcode;
		if (ManyBody || isopt || isfreq) {
			Coordinate[0] = ToLower(Coordinate[0]);
			Coordinate[1] = ToLower(Coordinate[1]);
			Coordinate[2] = ToLower(Coordinate[2]);
			using namespace fitting;
			string vsr[3] = { "rab", "rbc", "rac" };
			string vop[3] = { "c", "a", "b" };
			vector<Expr> g(3), ivg(3);
			int count = 0;
			for (size_t i = 0; i < Coordinate.size(); i++) {
				Coordinate[i] = ToUpper(Coordinate[i]);
				if (Coordinate[i][0] == 'r') {
					if (Coordinate[i].length() != 3) throw fitting::exception(("bad coordinate: " + Coordinate[i]));
					if (Coordinate[i][1] > Coordinate[i][2]) Coordinate[i] = string("r") + Coordinate[i][2] + Coordinate[i][1];
					for (size_t j = 0; j < 3; j++) 
						if (Coordinate[i] == vsr[j]) 
							g[j] = Expr::parse(Coordinate[i]), ivg[i] = Expr::parse(vsr[j]), count++;
				} else if (Coordinate[i][0] == 'x') {
					if (Coordinate[i].length() != 4) throw fitting::exception(("bad coordinate: " + Coordinate[i]));
					if (Coordinate[i][1] > Coordinate[i][3]) Coordinate[i] = string("x") + Coordinate[i][3] + Coordinate[i][2] + Coordinate[i][1];
				}
			}
			if (count == 2) {
				size_t k = 0, kj = 0; for (; !(g[k] == 0); k++);
				bool has = false;
				string dd = string("x") + vsr[k][1] + vop[k] + vsr[k][2];
				vector<Expr> dx, dxv;
				for (size_t j = 0; j < 3; j++) {
					if (Coordinate[j] == dd) {
						has = true; kj = j;
					} else dx.push_back(Expr::parse(Coordinate[j]));
				}
				if (!has) throw fitting::exception(("must have coordinate: " + dd));
				g[k] = "sqrt" % (dx[0] * dx[0] + dx[1] * dx[1] - 2 * dx[0] * dx[1] * ("cos" % (Expr::parse(dd) / 180.0 * Expr::parse("pi"))));
				dx.push_back(Expr::parse(vsr[k]));
				ivg[kj] = 180.0 / Expr::parse("pi") * ("acos" % ((dx[0] * dx[0] + dx[1] * dx[1] - dx[2] * dx[2]) / (2 * dx[0] * dx[1])));
			} else if (count == 1) {
				size_t k1 = 0; for (; !(g[k1] == 0); k1++); // k1, k2 是两个尚未赋值的内坐标指标
				size_t k2 = k1 + 1; for (; !(g[k2] == 0); k2++);
				size_t kk = 3 - k1 - k2; // kk 是已赋值的内坐标指标
				string aa = vop[kk], am = ""; // aa 是独立点的名称
				int h = 1;
				vector<Expr> dx(3);
				size_t jb, js, ja; // 主坐标的指标
				for (size_t j = 0; j < 3; j++) {
					if (Coordinate[j] == string("r") + aa + "m") 
						h *= 2, dx[0] = (Expr::parse(Coordinate[j])), jb = j; // j 是主坐标的指标，dx[0] 是 AM 的表达式
					else if (Coordinate[j] == string("x") + aa + "m" + vop[k1])
						am = vop[k1], h *= 3, dx[2] = (Expr::parse(Coordinate[j]) / 180.0 * Expr::parse("pi")), ja = j; // am 是构成角度的第三个点，dx[2] 是角度
					else if (Coordinate[j] == string("x") + aa + "m" + vop[k2])
						am = vop[k2], h *= 3, dx[2] = (Expr::parse(Coordinate[j]) / 180.0 * Expr::parse("pi")), ja = j;
					else if (Coordinate[j] == vsr[kk]) h *= 5, dx[1] = (Expr::parse(Coordinate[j])), js = j; // dx[1] 是已知边的表达式
					else throw fitting::exception(("Jacobi coordinates cannot be formed. " +
						Coordinate[0] + " " + Coordinate[1] + " " + Coordinate[2]));
				}
				if (h != 30) throw fitting::exception(("Jacobi coordinates cannot be formed. " +
					Coordinate[0] + " " + Coordinate[1] + " " + Coordinate[2]));
				string xs = "r" + am + aa;
				if (am[0] > aa[0]) { xs = "r" + aa + am; } // xs 是可以先求的边
				size_t k3 = xs == vsr[k1] ? k1 : k2; // k3 是可以先求的边的指标
				size_t k4 = xs != vsr[k1] ? k1 : k2;
				double km3 = AtomMass[am[0] - 'a'], km4 = AtomMass[3 - (am[0] - 'a') - (aa[0] - 'a')];
				Expr dm3 = dx[1] * (km4 / (km3 + km4)), dm4 = dx[1] * (km3 / (km3 + km4));
				g[k3] = "sqrt" % (dx[0] * dx[0] + dm3 * dm3 - 2 * dx[0] * dm3 * ("cos" % (dx[2])));
				g[k4] = "sqrt" % (dx[0] * dx[0] + dm4 * dm4 + 2 * dx[0] * dm4 * ("cos" % (dx[2])));
				ivg[js] = Expr::parse(vsr[kk]);
				Expr ek3 = Expr::parse(vsr[k3]), ek4 = Expr::parse(vsr[k4]), ekk = ivg[js];
				Expr ca4 = (ek3 * ek3 + ekk * ekk - ek4 * ek4) / (2 * ek3 * ekk);
				ivg[jb] = "sqrt" % (ek3 * ek3 + dm3 * dm3 - 2 * ek3 * dm3 * ca4);
				ivg[ja] = 180.0 / Expr::parse("pi") * ("acos" % ((dm3 - ek3 * ca4) / ivg[jb]));
			}

			if (ManyBody) {
				for (size_t j = 0; j < 3; j++)
					g[j] = g[j].simplify();

				vector<Expr> gd, gdd;
				for (size_t j = 0; j < 3; j++)
					for (size_t k = 0; k < 3; k++) {
						gd.push_back(g[j].diff(Coordinate[k]).expand().simplify());
						for (size_t l = k; l < 3; l++) {
							gdd.push_back(gd[gd.size() - 1].diff(Coordinate[l]).expand().simplify());
						}
					}

				mbcode << "#ifdef MANYBODY" << endl;
				mbcode << WriteCode("fh", Coordinate, g);
				mbcode << WriteCode("fhd", Coordinate, gd);
				mbcode << WriteCode("fhdd", Coordinate, gdd);
				mbcode << "#endif" << endl;
			}

			if (isfreq || isopt) {
				for (size_t j = 0; j < 3; j++)
					ivg[j] = ivg[j].simplify();

				vector<Expr> ivgd, ivgdd;
				for (size_t j = 0; j < 3; j++)
					for (size_t k = 0; k < 3; k++) {
						ivgd.push_back(ivg[j].diff(vsr[k]).simplify());
						for (size_t l = k; l < 3; l++) {
							ivgdd.push_back(ivgd[ivgd.size() - 1].diff(vsr[l]).simplify());
						}
					}

				vector<string> varp; varp.push_back("pi");
				vector<Expr> exprp; exprp.push_back(Expr(Expr::MPI));
				vector<string> vari(3); vari[0] = vsr[0], vari[1] = vsr[1], vari[2] = vsr[2];

				mbcode << "#if defined(FREQ) || defined(OPTM)" << endl;
				mbcode << WriteTrans("int_trans", vari, ivg, Coordinate, g, varp, exprp);
				mbcode << WriteDeriv("int_d", vari, ivgd, varp, exprp);
				mbcode << WriteDeriv("int_dd", vari, ivgdd, varp, exprp);
				mbcode << "#endif" << endl;
			}

			if (isfreq) {
				vector<Expr> h(9), ivh(3), ivhd, ivhdd;
				string car[9] = { "xa", "ya", "za", "xb", "yb", "zb", "xc", "yc", "zc" };
				Expr erab = Expr::parse("rab"), erbc = Expr::parse("rbc"), erac = Expr::parse("rac");
				Expr ama = "sqrt" % Expr::parse("ama"), amb = "sqrt" % Expr::parse("amb"), amc = "sqrt" % Expr::parse("amc");
				h[0] = h[1] = h[2] = 0.0;
				h[3] = erab * amb; h[4] = h[5] = 0.0;
				h[8] = 0.0;
				Expr cxabc = (erab*erab + erbc*erbc - erac*erac) / (2 * erab*erbc);
				Expr sxabc = "sqrt" % (1 - cxabc*cxabc);
				h[6] = (erab - erbc * cxabc) * amc;
				h[7] = erbc * sxabc * amc;
				Expr exa = Expr::parse("xa") / ama;
				Expr eya = Expr::parse("ya") / ama;
				Expr eza = Expr::parse("za") / ama;
				Expr exb = Expr::parse("xb") / amb;
				Expr eyb = Expr::parse("yb") / amb;
				Expr ezb = Expr::parse("zb") / amb;
				Expr exc = Expr::parse("xc") / amc;
				Expr eyc = Expr::parse("yc") / amc;
				Expr ezc = Expr::parse("zc") / amc;
				ivh[0] = "sqrt" % ((exa - exb)*(exa - exb) + (eya - eyb)*(eya - eyb) + (eza - ezb)*(eza - ezb));
				ivh[1] = "sqrt" % ((exc - exb)*(exc - exb) + (eyc - eyb)*(eyc - eyb) + (ezc - ezb)*(ezc - ezb));
				ivh[2] = "sqrt" % ((exa - exc)*(exa - exc) + (eya - eyc)*(eya - eyc) + (eza - ezc)*(eza - ezc));
				for (size_t j = 0; j < 3; j++)
					for (size_t k = 0; k < 9; k++) {
						ivhd.push_back(ivh[j].diff(car[k]).simplify());
						for (size_t l = k; l < 9; l++) {
							ivhdd.push_back(ivhd[ivhd.size() - 1].diff(car[l]).simplify());
						}
					}

				vector<string> varp; varp.push_back("ama"); varp.push_back("amb"); varp.push_back("amc");
				vector<Expr> exprp; exprp.push_back(Expr::parse("AMA")); exprp.push_back(Expr::parse("AMB")); 
				exprp.push_back(Expr::parse("AMC"));
				vector<string> varx(3), vari(9); varx[0] = vsr[0], varx[1] = vsr[1], varx[2] = vsr[2];
				for (size_t j = 0; j < 9; j++) vari[j] = car[j];

				mbcode << "#ifdef FREQ" << endl;
				mbcode << WriteTrans("car_trans", vari, ivh, varx, h, varp, exprp);
				mbcode << WriteDeriv("car_d", vari, ivhd, varp, exprp);
				mbcode << WriteDeriv("car_dd", vari, ivhdd, varp, exprp);
				mbcode << "#endif" << endl;
			}

		}

		if (ispoint || isfreq || isopt) {

			mbcode << "#ifdef POINT" << endl;
			mbcode << "program intpol_p" << endl;
			mbcode << "  implicit none" << endl;
			mbcode << "  real(8) :: x(0:2)" << endl;
			if (isopt) {
				mbcode << "  integer :: iter, root, ierr" << endl;
				mbcode << "  real(8) :: step_len, step_conv, f_conv" << endl;
			}
			mbcode << endl;
			mbcode << "  call run_init()" << endl;
			mbcode << "  open (15, file = PARAMFILE)" << endl;
			mbcode << endl;
			int ipoint = 1, iopt = 1;
			for (size_t i = 0; i < tasks.size(); i++) {
				if (tasks[i].type == 1) {
					if (tasks[i].point.size() != 0) {
						string tname = "point" + IntToString(ipoint++);
						pcode << "&" << tname << endl << "  x = ";
						pcode << DToStrF(tasks[i].point[0]) << ", ";
						pcode << DToStrF(tasks[i].point[1]) << ", ";
						pcode << DToStrF(tasks[i].point[2]) << endl << "&end" << endl;
						mbcode << "  namelist /" << tname << "/ x" << endl;
						mbcode << "  read (15, " << tname << ")" << endl;
					}
					mbcode << "  call run_point(x)" << endl;
					mbcode << endl;
				} else if (tasks[i].type == 2) {
					string tname = "opt" + IntToString(iopt++);
					pcode << "&" << tname << endl;
					pcode << "  iter = " << tasks[i].n_step << endl;
					pcode << "  root = " << tasks[i].opt_type << endl;
					pcode << "  step_len = " << tasks[i].st_max << endl;
					pcode << "  step_conv = " << tasks[i].st_conv << endl;
					pcode << "  f_conv = " << tasks[i].g_conv << endl;
					pcode << "&end" << endl;
					mbcode << "  namelist /" << tname << "/ iter, root, step_len, step_conv, f_conv" << endl;
					mbcode << "  read (15, " << tname << ")" << endl;
					mbcode << "  call run_opt(x, iter, root, step_len, step_conv, f_conv, ierr)" << endl;
					mbcode << "  if (ierr /= 0) goto 7" << endl;
					mbcode << endl;
				} else if (tasks[i].type == 3) {
					mbcode << "  call run_freq(x)" << endl;
					mbcode << endl;
				}
			}
			mbcode << "7 close (15)" << endl;
			mbcode << "  call run_end()" << endl;
			mbcode << endl;
			mbcode << "end program intpol_p" << endl;
			mbcode << "#endif" << endl;

		}

		if (!FileOrPathExists(OutputDir)) MakeDirectory(OutputDir);

		stringstream def;
		def << "" << endl << "! head file generated for INTPOL" << endl << "! System: " <<
			AtomName[0] << " " << AtomName[1] << " " << AtomName[2] << endl;
		def << "! Mthod: " << Method << endl;
		def << "! Manybody: " << (ManyBody ? "True" : "False") << endl << endl;
		for (size_t i = 0; i < defines.size(); i++) {
			if (defines[i].first[0] == '#') def << defines[i].first << endl;
			else if (defines[i].second.length() == 0) def << "#define " + defines[i].first << endl;
			else def << "#define " + defines[i].first << " " << defines[i].second << endl;
		}
		def << endl << mbcode.str() << endl;
		WriteFileContent(OutputDir + "/head.h", def.str());

		stringstream ruf;
		ruf << "#!/bin/bash" << endl << endl;
		ruf << "if [ `which icpc 2>&1 | wc -w` -ne 1 ]; then" << endl;
		ruf << "  CPP=cpp" << endl << "else" << endl << "  CPP='icpc -E'" << endl << "fi" << endl;
		ruf << "if [ `which ifort 2>&1 | wc -w` -ne 1 ]; then" << endl;
		ruf << "  FC='gfortran -fopenmp'" << endl << "  FCL='gfortran -fopenmp -lblas -llapack'" << endl;
		ruf << "else" << endl << "  FC='ifort -openmp'" << endl << "  FCL='ifort -openmp -mkl'" << endl << "fi" << endl;
		ruf << endl;
		ruf << "$CPP -DPREP template.f90 | sed '/^#/d' | cat -s > prep.f90" << endl;
		ruf << "$CPP -DTEST template.f90 | sed '/^#/d' | cat -s > test.f90" << endl;
		if (ispoint) ruf << "$CPP -DRUN template.f90 | sed '/^#/d' | cat -s > run.f90" << endl;
		ruf << "$CPP template.f90 | sed '/^#/d' | cat -s > calc.f90" << endl;
		ruf << endl;
		ruf << "$FC -O3 prep.f90 -o prep.x" << endl;
		ruf << "$FC -O3 test.f90 -o test.x" << endl;
		if (isopt || isfreq) ruf << "$FCL -O3 run.f90 -o run.x" << endl;
		else if (ispoint) ruf << "$FC -O3 run.f90 -o run.x" << endl;
		ruf << endl;
		WriteFileContent(OutputDir + "/compile.sh", ruf.str());
		ChangeMode(OutputDir + "/compile.sh", 0755);

		if (ManyBody) {
			for (size_t k = 0; k < 3; k++) {
				stringstream mbf;
				for (size_t i = 0; i < mb_rx[k].size(); i++) {
					mbf.width(9);
					mbf << (i + 1);
					mbf.precision(6);
					mbf.width(15);
					mbf << mb_rx[k][i][0];
					mbf.width(27);
					mbf.precision(16);
					mbf << mb_rx[k][i][1] << endl;
				}
				WriteFileContent(OutputDir + "/" + smbxf[k], mbf.str());
			}
		}

		stringstream maf; maf.precision(10);
		for (size_t i = 0; i < main_rx.size(); i++) {
			maf.width(9);
			maf << (i + 1);
			maf.precision(6);
			maf.width(15);
			maf << main_rx[i][0];
			maf.width(15);
			maf << main_rx[i][1];
			maf.width(15);
			maf << main_rx[i][2];
			maf.width(27);
			maf.precision(16);
			maf << main_rx[i][3] << endl;
		}
		WriteFileContent(OutputDir + "/" + smaf, maf.str());

		stringstream paf; paf.precision(10);
		paf.precision(6);
		paf << "&rkhs" << endl;
		paf << "  r_exp = " << exps[0] << ", " << exps[1] << ", " << exps[2] << ", " << exps[3] << endl;
		paf << "  r_alpha = " << regs[0] << ", " << regs[1] << ", " << regs[2] << ", " << regs[3] << endl;
		paf << "&end" << endl;
		paf << pcode.str();
		WriteFileContent(OutputDir + "/" + spaf, paf.str());

		WriteFileContent(OutputDir + "/template.f90", ReadFileContent("template.f90"));
	}

}

int main(int argc, char *argv[]) {
	if (argc != 2)
		cout << "Usage: " << argv[0] << " input-file" << endl;
	else {
		try {
			ifstream ifs(argv[1]);
			const vector<pair<string, MapSV> >& ip = RawParse(ReadStream(&ifs));
			Intpol::Run(ip);
		} catch (fitting::exception e) {
			cout << e.g << endl;
		}
	}
	cout << "all finished." << endl;
	// int h; cin >> h;
	return 0;
}
