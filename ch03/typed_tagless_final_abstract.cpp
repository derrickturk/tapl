// the typed tagless final abstract approach,
//   roughly ported to C++17

#include <iostream>
#include <string>

struct eval {
    static constexpr bool t = true;
    static constexpr bool f = false;
    
    template<class T>
    static constexpr T ifThenElse(bool cond, T t, T f) noexcept
    {
        return cond ? t : f;
    }

    static constexpr int z = 0;

    static constexpr int succ(int n) noexcept { return n + 1; }

    static constexpr int pred(int n) noexcept { return (n == 0) ? 0 : (n - 1); }

    static constexpr bool isZero(int n) noexcept { return n == 0; }
};

struct pprint {
    inline static const std::string t = "t";
    inline static const std::string f = "f";

    static std::string ifThenElse(const std::string& cond,
            const std::string& t, const std::string& f)
    {
        return "ifThenElse(" + cond + ", " + t + ", " + f + ")";
    }

    inline static const std::string z = "z";

    static std::string succ(const std::string& n)
    {
        return "succ(" + n + ")";
    }

    static std::string pred(const std::string& n)
    {
        return "pred(" + n + ")";
    }

    static std::string isZero(const std::string& n)
    {
        return "isZero(" + n + ")";
    }
};

template<class Interp> auto exampleExpr =
  Interp::ifThenElse(
    Interp::isZero(Interp::succ(Interp::z)),
    Interp::pred(Interp::succ(Interp::z)),
    Interp::succ(Interp::succ(Interp::z)));

int main()
{
    std::cout << exampleExpr<eval> << '\n';
    std::cout << exampleExpr<pprint> << '\n';
}
