#include <cassert>
#include <cctype>
#include <cstdint>
#include <functional>
#include <iostream>
#include <memory>
#include <random>
#include <string>
#include <string_view>
#include <vector>

constexpr std::size_t kWordBits = 32;
using Word = std::uint32_t;

constexpr Word Mask() noexcept
{
    if constexpr (kWordBits == 64)
    {
        return ~0ULL;
    }
    else
    {
        return (static_cast<Word>(1) << kWordBits) - 1;
    }
}

enum class Operator { And, Or, Xor, Plus, Minus, Mul, ShiftL, ShiftR, Modulo };
enum class UnaryOperator { Not, Negate, RotR };
enum class NodeType { Immediate, Variable, UnaryExpr, BinaryExpr };

struct Node
{
    NodeType m_type = NodeType::Immediate;
    Word m_value = 0;
    char m_varName = '\0';

    UnaryOperator m_unaryOp = UnaryOperator::Not;
    std::unique_ptr<Node> m_unaryChild;

    Operator m_binaryOp = Operator::Plus;
    std::unique_ptr<Node> m_lhs;
    std::unique_ptr<Node> m_rhs;

    Node() = default;

    Node(const Node& other)
    {
        *this = other;
    }
    Node& operator=(const Node& other)
    {
        if (this == &other)
        {
            return *this;
        }
        m_type = other.m_type;
        m_value = other.m_value;
        m_varName = other.m_varName;
        m_unaryOp = other.m_unaryOp;
        m_binaryOp = other.m_binaryOp;
        m_unaryChild = other.m_unaryChild ? std::make_unique<Node>(*other.m_unaryChild) : nullptr;
        m_lhs = other.m_lhs ? std::make_unique<Node>(*other.m_lhs) : nullptr;
        m_rhs = other.m_rhs ? std::make_unique<Node>(*other.m_rhs) : nullptr;
        return *this;
    }
    Node(Node&&) = default;
    Node& operator=(Node&&) = default;

    static Node MakeImmediate(Word value)
    {
        Node newNode;
        newNode.m_type = NodeType::Immediate;
        newNode.m_value = value;
        return newNode;
    }
    static Node MakeVariable(char character)
    {
        Node newNode;
        newNode.m_type = NodeType::Variable;
        newNode.m_varName = character;
        newNode.m_value = static_cast<Word>(character);
        return newNode;
    }
    static Node MakeUnaryExpr(UnaryOperator unaryOp, Node child)
    {
        Node newNode;
        newNode.m_type = NodeType::UnaryExpr;
        newNode.m_unaryOp = unaryOp;
        newNode.m_unaryChild = std::make_unique<Node>(std::move(child));
        return newNode;
    }
    static Node MakeBinaryExpr(Operator binaryOp, Node left, Node right)
    {
        Node newNode;
        newNode.m_type = NodeType::BinaryExpr;
        newNode.m_binaryOp = binaryOp;
        newNode.m_lhs = std::make_unique<Node>(std::move(left));
        newNode.m_rhs = std::make_unique<Node>(std::move(right));
        return newNode;
    }
};

std::string OperatorToString(Operator op)
{
    switch (op)
    {
    case Operator::And: return "&";
    case Operator::Or: return "|";
    case Operator::Xor: return "^";
    case Operator::Plus: return "+";
    case Operator::Minus: return "-";
    case Operator::Mul: return "*";
    case Operator::ShiftL: return "<<";
    case Operator::ShiftR: return ">>";
    case Operator::Modulo: return "%";
    }
    return "?";
}

std::string UnaryOperatorToString(UnaryOperator u)
{
    switch (u)
    {
    case UnaryOperator::Not: return "~";
    case UnaryOperator::Negate: return "-";
    case UnaryOperator::RotR: return "ror";
    }
    return "?";
}

std::string NodeToString(const Node& node)
{
    switch (node.m_type)
    {
    case NodeType::Immediate: return std::to_string(node.m_value);
    case NodeType::Variable: return std::string(1, node.m_varName);
    case NodeType::UnaryExpr: return UnaryOperatorToString(node.m_unaryOp) + "(" + NodeToString(*node.m_unaryChild) + ")";
    case NodeType::BinaryExpr: return "(" + NodeToString(*node.m_lhs) + " " + OperatorToString(node.m_binaryOp) + " " + NodeToString(*node.m_rhs) + ")";
    }
    return "?";
}

Word EvalNode(const Node& node)
{
    switch (node.m_type)
    {
    case NodeType::Immediate: return node.m_value & Mask();
    case NodeType::Variable: return node.m_value & Mask();
    case NodeType::UnaryExpr:
    {
        Word childValue = EvalNode(*node.m_unaryChild);
        switch (node.m_unaryOp)
        {
        case UnaryOperator::Not: return ~childValue & Mask();
        case UnaryOperator::Negate: return (~childValue + 1) & Mask();
        case UnaryOperator::RotR:
        {
            constexpr unsigned int shiftAmount = 1;
            return ((childValue >> shiftAmount) | (childValue << (kWordBits - shiftAmount))) & Mask();
        }
        }
    }
    case NodeType::BinaryExpr:
    {
        Word leftValue = EvalNode(*node.m_lhs);
        Word rightValue = EvalNode(*node.m_rhs);
        switch (node.m_binaryOp)
        {
        case Operator::And: return (leftValue & rightValue) & Mask();
        case Operator::Or:  return (leftValue | rightValue) & Mask();
        case Operator::Xor: return (leftValue ^ rightValue) & Mask();
        case Operator::Plus: return (leftValue + rightValue) & Mask();
        case Operator::Minus:return (leftValue - rightValue) & Mask();
        case Operator::Mul:  return (leftValue * rightValue) & Mask();
        case Operator::ShiftL: return (leftValue << rightValue) & Mask();
        case Operator::ShiftR: return (leftValue >> rightValue) & Mask();
        case Operator::Modulo:
            if (rightValue == 0) // Éviter la division par zéro
            {
                return 0;
            }
            return (leftValue % rightValue) & Mask();
        }
    }
    }
    return 0;
}

namespace Rng
{
    std::random_device s_randomDevice;
    std::mt19937 s_generator(s_randomDevice());
}

Node ObfuscateNode(const Node& node);

Node ObfuscateBinary(Operator op, const Node& nodeA, const Node& nodeB)
{
    Node obfuscatedNodeA = ObfuscateNode(nodeA);
    Node obfuscatedNodeB = ObfuscateNode(nodeB);
    using TransformFunction = std::function<Node(Node, Node)>;

    switch (op)
    {
    case Operator::Xor:
    {
        std::vector<TransformFunction> choices = {
            [](Node left, Node right) {
                Node orNode = Node::MakeBinaryExpr(Operator::Or, left, right);
                Node andNode = Node::MakeBinaryExpr(Operator::And, std::move(left), std::move(right));
                return Node::MakeBinaryExpr(Operator::Minus, std::move(orNode), std::move(andNode));
            },
            [](Node left, Node right) {
                Node notRightNode = Node::MakeUnaryExpr(UnaryOperator::Not, right);
                Node term1Node = Node::MakeBinaryExpr(Operator::And, left, std::move(notRightNode));
                Node notLeftNode = Node::MakeUnaryExpr(UnaryOperator::Not, std::move(left));
                Node term2Node = Node::MakeBinaryExpr(Operator::And, std::move(notLeftNode), std::move(right));
                return Node::MakeBinaryExpr(Operator::Or, std::move(term1Node), std::move(term2Node));
            }
        };
        std::uniform_int_distribution<> distribution(0, static_cast<int>(choices.size() - 1));
        return choices[distribution(Rng::s_generator)](std::move(obfuscatedNodeA), std::move(obfuscatedNodeB));
    }
    case Operator::Plus:
    {
        std::vector<TransformFunction> choices = {
            [](Node left, Node right) {
                Node xorNode = Node::MakeBinaryExpr(Operator::Xor, left, right);
                Node andNode = Node::MakeBinaryExpr(Operator::And, std::move(left), std::move(right));
                Node term2Node = Node::MakeBinaryExpr(Operator::Mul, Node::MakeImmediate(2), std::move(andNode));
                return Node::MakeBinaryExpr(Operator::Plus, std::move(xorNode), std::move(term2Node));
            },
            [](Node left, Node right) {
                Node orNode = Node::MakeBinaryExpr(Operator::Or, left, right);
                Node andNode = Node::MakeBinaryExpr(Operator::And, std::move(left), std::move(right));
                return Node::MakeBinaryExpr(Operator::Plus, std::move(orNode), std::move(andNode));
            }
        };
        std::uniform_int_distribution<> distribution(0, static_cast<int>(choices.size() - 1));
        return choices[distribution(Rng::s_generator)](std::move(obfuscatedNodeA), std::move(obfuscatedNodeB));
    }
    case Operator::Minus:
    {
        Node notBNode = Node::MakeUnaryExpr(UnaryOperator::Not, obfuscatedNodeB);
        Node plusOneNode = Node::MakeImmediate(1);
        return Node::MakeBinaryExpr(Operator::Plus, obfuscatedNodeA, Node::MakeBinaryExpr(Operator::Plus, std::move(notBNode), std::move(plusOneNode)));
    }
    case Operator::Or:
    {
        std::vector<TransformFunction> choices = {
            [](Node left, Node right) {
                Node xorNode = Node::MakeBinaryExpr(Operator::Xor, left, right);
                Node andNode = Node::MakeBinaryExpr(Operator::And, std::move(left), std::move(right));
                return Node::MakeBinaryExpr(Operator::Plus, std::move(xorNode), std::move(andNode));
            }
        };
        std::uniform_int_distribution<> distribution(0, static_cast<int>(choices.size() - 1));
        return choices[distribution(Rng::s_generator)](std::move(obfuscatedNodeA), std::move(obfuscatedNodeB));
    }
    case Operator::And:
    {
        std::vector<TransformFunction> choices = {
            [](Node left, Node right) {
                Node orNode = Node::MakeBinaryExpr(Operator::Or, left, right);
                Node xorNode = Node::MakeBinaryExpr(Operator::Xor, std::move(left), std::move(right));
                return Node::MakeBinaryExpr(Operator::Minus, std::move(orNode), std::move(xorNode));
            }
        };
        std::uniform_int_distribution<> distribution(0, static_cast<int>(choices.size() - 1));
        return choices[distribution(Rng::s_generator)](std::move(obfuscatedNodeA), std::move(obfuscatedNodeB));
    }

    case Operator::ShiftL:
    {
        Node powerOfTwoNode = Node::MakeBinaryExpr(Operator::ShiftL, Node::MakeImmediate(1), obfuscatedNodeB);
        return Node::MakeBinaryExpr(Operator::Mul, obfuscatedNodeA, std::move(powerOfTwoNode));
    }
    case Operator::ShiftR:
    case Operator::Modulo:
    case Operator::Mul:
    default:
        // pas d'obfuscation «simple»
        return Node::MakeBinaryExpr(op, std::move(obfuscatedNodeA), std::move(obfuscatedNodeB));
    }
}

Node ObfuscateNode(const Node& node)
{
    switch (node.m_type)
    {
    case NodeType::Immediate: return node;
    case NodeType::Variable:  return node;

    case NodeType::UnaryExpr:
    {
        Node obfuscatedChild = ObfuscateNode(*node.m_unaryChild);
        if (node.m_unaryOp == UnaryOperator::RotR)
        {
            // ror(x, 1)  <=>  (x >> 1) | (x << (WordBits - 1))
            Node shiftRightNode = Node::MakeBinaryExpr(Operator::ShiftR, obfuscatedChild, Node::MakeImmediate(1));
            Node shiftLeftNode = Node::MakeBinaryExpr(Operator::ShiftL, std::move(obfuscatedChild), Node::MakeImmediate(kWordBits - 1));
            return Node::MakeBinaryExpr(Operator::Or, std::move(shiftRightNode), std::move(shiftLeftNode));
        }
        // obfusque juste l'enfant
        return Node::MakeUnaryExpr(node.m_unaryOp, std::move(obfuscatedChild));
    }

    case NodeType::BinaryExpr:
        return ObfuscateBinary(node.m_binaryOp, *node.m_lhs, *node.m_rhs);
    }
    return node;
}

enum class Tok
{
    End, Lp, Rp, Not, Minus, Plus, Star, Amp, Pipe, Caret, Num, Id,
    ShiftL, ShiftR, Mod, Ror
};
struct Token { Tok m_type; Word m_value{}; char m_id{}; };

struct Lexer
{
    std::string_view m_source;
    std::size_t m_position = 0;

    void SkipWhitespace()
    {
        while (m_position < m_source.size() && std::isspace(static_cast<unsigned char>(m_source[m_position])))
        {
            ++m_position;
        }
    }

    Token GetNextToken()
    {
        SkipWhitespace();
        if (m_position >= m_source.size())
        {
            return { Tok::End };
        }
        char currentChar = m_source[m_position++];
        switch (currentChar)
        {
            case '(': return{ Tok::Lp };    case ')': return{ Tok::Rp };
            case '~': return{ Tok::Not };   case '-': return{ Tok::Minus };
            case '+': return{ Tok::Plus };  case '*': return{ Tok::Star };
            case '&': return{ Tok::Amp };   case '|': return{ Tok::Pipe };
            case '^': return{ Tok::Caret };
            case '%': return{ Tok::Mod };
            case '<':
                if (m_position < m_source.size() && m_source[m_position] == '<')
                {
                    m_position++; return { Tok::ShiftL };
                }
                break;
            case '>':
                if (m_position < m_source.size() && m_source[m_position] == '>')
                {
                    m_position++; return { Tok::ShiftR };
                }
                break;
        }

        if (std::isdigit(static_cast<unsigned char>(currentChar)))
        {
            Word numericValue = currentChar - '0';
            while (m_position < m_source.size() && std::isdigit(static_cast<unsigned char>(m_source[m_position])))
            {
                numericValue = numericValue * 10 + (m_source[m_position++] - '0');
            }
            return{ Tok::Num, numericValue };
        }
        if (std::isalpha(static_cast<unsigned char>(currentChar)))
        {
            m_position--;
            std::string identifier;
            while (m_position < m_source.size() && std::isalpha(static_cast<unsigned char>(m_source[m_position])))
            {
                identifier += m_source[m_position++];
            }
            if (identifier == "ror")
            {
                return { Tok::Ror };
            }
            if (identifier.length() == 1)
            {
                return{ Tok::Id, 0, identifier[0] };
            }
        }
        return GetNextToken();
    }
};

struct Parser
{
    Lexer m_lexer;
    Token m_currentToken;

    explicit Parser(std::string_view source) : m_lexer{ source }
    {
        m_currentToken = m_lexer.GetNextToken();
    }
    void ConsumeToken(Tok expectedType)
    {
        if (m_currentToken.m_type == expectedType)
        {
            m_currentToken = m_lexer.GetNextToken();
        }
    }

    Node ParseExpression() { return ParseBinaryOr(); }
    Node ParseBinaryOr()
    {
        Node leftNode = ParseBinaryXor();
        while (m_currentToken.m_type == Tok::Pipe)
        {
            ConsumeToken(Tok::Pipe);
            leftNode = Node::MakeBinaryExpr(Operator::Or, leftNode, ParseBinaryXor());
        }
        return leftNode;
    }
    Node ParseBinaryXor()
    {
        Node leftNode = ParseBinaryAnd();
        while (m_currentToken.m_type == Tok::Caret)
        {
            ConsumeToken(Tok::Caret);
            leftNode = Node::MakeBinaryExpr(Operator::Xor, leftNode, ParseBinaryAnd());
        }
        return leftNode;
    }
    Node ParseBinaryAnd()
    {
        Node leftNode = ParseAdditionSubtraction();
        while (m_currentToken.m_type == Tok::Amp)
        {
            ConsumeToken(Tok::Amp);
            leftNode = Node::MakeBinaryExpr(Operator::And, leftNode, ParseAdditionSubtraction());
        }
        return leftNode;
    }

    Node ParseAdditionSubtraction()
    {
        Node leftNode = ParseShift();
        while (m_currentToken.m_type == Tok::Plus || m_currentToken.m_type == Tok::Minus)
        {
            Operator currentOp = (m_currentToken.m_type == Tok::Plus) ? Operator::Plus : Operator::Minus;
            ConsumeToken(m_currentToken.m_type);
            leftNode = Node::MakeBinaryExpr(currentOp, leftNode, ParseShift());
        }
        return leftNode;
    }

    Node ParseShift()
    {
        Node leftNode = ParseMultiplicationDivision();
        while (m_currentToken.m_type == Tok::ShiftL || m_currentToken.m_type == Tok::ShiftR)
        {
            Operator currentOp = (m_currentToken.m_type == Tok::ShiftL) ? Operator::ShiftL : Operator::ShiftR;
            ConsumeToken(m_currentToken.m_type);
            leftNode = Node::MakeBinaryExpr(currentOp, leftNode, ParseMultiplicationDivision());
        }
        return leftNode;
    }

    Node ParseMultiplicationDivision()
    {
        Node leftNode = ParseUnary();
        while (m_currentToken.m_type == Tok::Star || m_currentToken.m_type == Tok::Mod)
        {
            Operator currentOp = (m_currentToken.m_type == Tok::Star) ? Operator::Mul : Operator::Modulo;
            ConsumeToken(m_currentToken.m_type);
            leftNode = Node::MakeBinaryExpr(currentOp, leftNode, ParseUnary());
        }
        return leftNode;
    }

    Node ParseUnary()
    {
        if (m_currentToken.m_type == Tok::Not) { ConsumeToken(Tok::Not);   return Node::MakeUnaryExpr(UnaryOperator::Not, ParseUnary()); }
        if (m_currentToken.m_type == Tok::Minus) { ConsumeToken(Tok::Minus); return Node::MakeUnaryExpr(UnaryOperator::Negate, ParseUnary()); }
        if (m_currentToken.m_type == Tok::Ror) { ConsumeToken(Tok::Ror);   return Node::MakeUnaryExpr(UnaryOperator::RotR, ParseUnary()); }
        return ParsePrimary();
    }

    Node ParsePrimary()
    {
        if (m_currentToken.m_type == Tok::Num)
        {
            Word value = m_currentToken.m_value;
            ConsumeToken(Tok::Num);
            return Node::MakeImmediate(value);
        }
        if (m_currentToken.m_type == Tok::Id)
        {
            char idChar = m_currentToken.m_id;
            ConsumeToken(Tok::Id);
            return Node::MakeVariable(idChar);
        }
        if (m_currentToken.m_type == Tok::Lp)
        {
            ConsumeToken(Tok::Lp);
            Node expressionNode = ParseExpression();
            ConsumeToken(Tok::Rp);
            return expressionNode;
        }
        return Node::MakeImmediate(0);
    }
};

// c'est nul mais pour test
Word CalculateHash(const std::string& inputString)
{
    Word currentHash = 0;
    for (char character : inputString)
    {
        currentHash = (currentHash * 31) + static_cast<Word>(character);
    }
    return currentHash & Mask(); // 32 bits
}

void SetVariableValue(Node& node, char variableName, Word value)
{
    switch (node.m_type)
    {
    case NodeType::Variable:
        if (node.m_varName == variableName)
        {
            node.m_value = value;
        }
        break;
    case NodeType::UnaryExpr:
        SetVariableValue(*node.m_unaryChild, variableName, value);
        break;
    case NodeType::BinaryExpr:
        SetVariableValue(*node.m_lhs, variableName, value);
        SetVariableValue(*node.m_rhs, variableName, value);
        break;
    default:
        break;
    }
}

std::string CppOperatorToString(Operator op)
{
    switch (op)
    {
        case Operator::And: return "&";
        case Operator::Or: return "|";
        case Operator::Xor: return "^";
        case Operator::Plus: return "+";
        case Operator::Minus: return "-";
        case Operator::Mul: return "*";
        case Operator::ShiftL: return "<<";
        case Operator::ShiftR: return ">>";
        case Operator::Modulo: return "%";
    }
    return "?";
}

std::string CppUnaryOperatorToString(UnaryOperator u)
{
    switch (u)
    {
        case UnaryOperator::Not: return "~";
        case UnaryOperator::Negate: return "-";
        case UnaryOperator::RotR: return "ror_func";
    }
    return "?";
}

std::string NodeToCppCode(const Node& node)
{
    switch (node.m_type)
    {
    case NodeType::Immediate:
        return "static_cast<Word>(" + std::to_string(node.m_value) + "U)";

    case NodeType::Variable:
        return std::string(1, node.m_varName);

    case NodeType::UnaryExpr:
        if (node.m_unaryOp == UnaryOperator::RotR)
        {
            return "ror_func(" + NodeToCppCode(*node.m_unaryChild) + ")";
        }
        return "(" + CppUnaryOperatorToString(node.m_unaryOp) + "(" + NodeToCppCode(*node.m_unaryChild) + "))";

    case NodeType::BinaryExpr:
        return "((" + NodeToCppCode(*node.m_lhs) + ") " + CppOperatorToString(node.m_binaryOp) + " (" + NodeToCppCode(*node.m_rhs) + "))";
    }
    return "static_cast<Word>(0)";
}

int RunTest()
{
    // std::cout << CalculateHash("KISS");
    std::cout << "Entrez une expression > ";

    std::string inputLine;
    if (!std::getline(std::cin, inputLine))
    {
        return 1;
    }

    Parser parser(inputLine);
    Node abstractSyntaxTree = parser.ParseExpression();
    Word referenceValue = EvalNode(abstractSyntaxTree);

    std::cout << "\n--------------------------------------------------\n";
    std::cout << "AST                 : " << NodeToString(abstractSyntaxTree) << "\n";
    std::cout << "Valeur de reference : " << referenceValue << '\n';
    std::cout << "--------------------------------------------------\n";

    Node currentNode = abstractSyntaxTree;
    for (int i = 1; i <= 3; ++i)
    {
        currentNode = ObfuscateNode(currentNode);
        Word obfuscatedValue = EvalNode(currentNode);

        std::cout << "\n[Niveau d'obfuscation " << i << "]\n";
        std::cout << "AST Obfusque: " << NodeToString(currentNode) << "\n";
        std::cout << "Valeur      = " << obfuscatedValue << '\n';

        assert(obfuscatedValue == referenceValue && "La valeur a change apres obfuscation.");
    }

    return 0;
}

int GenerateCppCode()
{
    constexpr Word kTargetHash = 4166625696;

    Node verificationAst = Node::MakeBinaryExpr(
        Operator::Xor,
        Node::MakeVariable('h'),
        Node::MakeImmediate(kTargetHash)
    );

    int obfuscationLevel = 3;
    for (int i = 0; i < obfuscationLevel; ++i)
    {
        verificationAst = ObfuscateNode(verificationAst);
    }

    std::cout << "#include <cstdint>\n\n";
    std::cout << "using Word = std::uint32_t;\n\n";

    std::cout << "Word ror_func(Word val) {\n";
    std::cout << "    constexpr unsigned int WordBits = 32;\n"; 
    std::cout << "    return (val >> 1) | (val << (WordBits - 1));\n";
    std::cout << "}\n\n";

    std::cout << "Word check_password(Word h) {\n";
    std::cout << "    return " << NodeToCppCode(verificationAst) << ";\n";
    std::cout << "}\n\n";
    std::cout << "// --------------------------------------------------\n";

    return EXIT_SUCCESS;
}

int main()
{
    RunTest();
    //GenerateCppCode();

    return EXIT_SUCCESS;
}
