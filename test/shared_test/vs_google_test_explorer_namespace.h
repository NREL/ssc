// vs_google_test_explorer_namespace.h
//
// This is an example of a file/macro that could be used to trick the Google Test Adapter into
// sorting tests into namespaces in C++
//
// Note addition of 'int' on line 39 to fix googletest error (MB)
//
// https://bitbucket.org/CadActive/workspace/snippets/GrBakB

#ifndef NAMESPACE_GTEST_INCLUDE_GTEST_GTEST_H_
#define NAMESPACE_GTEST_INCLUDE_GTEST_GTEST_H_

#include <gtest/gtest.h>

// ---------------------------------------------------------------
// Simple Namespace
// - .
// - Pass simple namespace name as an arguments to the Macro
// - This only supports a single namespace of 'depth'
// 
// - The class foo::ClassName could be represented as
//   NAMESPACE_TEST(foo, ClassName, TestName)
//
// ---------------------------------------------------------------

// A copy of GTEST_TEST_CLASS_NAME_, but with handling for namespace name
#define NAMESPACE_GTEST_TEST_CLASS_NAME_(namespace_name, test_case_name, test_name) \
  namespace_name##_##test_case_name##_##test_name##_Test

// A copy of GTEST_TEST_, but with handling for namespace name.
#if defined(WIN32) || defined(_WIN32) || defined(WIN32) || defined(NT)
#define NAMESPACE_GTEST_TEST_(namespace_name, test_case_name, test_name, parent_class, parent_id)\
class NAMESPACE_GTEST_TEST_CLASS_NAME_(namespace_name, test_case_name, test_name) : public parent_class {\
 public:\
  NAMESPACE_GTEST_TEST_CLASS_NAME_(namespace_name, test_case_name, test_name)() {}\
 private:\
  virtual void TestBody();\
  static ::testing::TestInfo* const test_info_ GTEST_ATTRIBUTE_UNUSED_;\
  int GTEST_DISALLOW_COPY_AND_ASSIGN_(\
      NAMESPACE_GTEST_TEST_CLASS_NAME_(namespace_name, test_case_name, test_name));\
};\
\
::testing::TestInfo* const NAMESPACE_GTEST_TEST_CLASS_NAME_(namespace_name, test_case_name, test_name)\
  ::test_info_ =\
    ::testing::internal::MakeAndRegisterTestInfo(\
        #namespace_name "." #test_case_name, #test_name, NULL, NULL, /* <-- Defines the test as "Namespace.Classname" */ \
        ::testing::internal::CodeLocation(__FILE__, __LINE__), \
        (parent_id), \
        parent_class::SetUpTestCase, \
        parent_class::TearDownTestCase, \
        new ::testing::internal::TestFactoryImpl<\
            NAMESPACE_GTEST_TEST_CLASS_NAME_(namespace_name, test_case_name, test_name)>);\
void NAMESPACE_GTEST_TEST_CLASS_NAME_(namespace_name, test_case_name, test_name)::TestBody()
#else
#define NAMESPACE_GTEST_TEST_(namespace_name, test_case_name, test_name, parent_class, parent_id)\
class NAMESPACE_GTEST_TEST_CLASS_NAME_(namespace_name, test_case_name, test_name) : public parent_class {\
 public:\
  NAMESPACE_GTEST_TEST_CLASS_NAME_(namespace_name, test_case_name, test_name)() {}\
 private:\
  virtual void TestBody();\
  static ::testing::TestInfo* const test_info_ GTEST_ATTRIBUTE_UNUSED_;\
  GTEST_DISALLOW_COPY_AND_ASSIGN_(\
      NAMESPACE_GTEST_TEST_CLASS_NAME_(namespace_name, test_case_name, test_name));\
};\
\
::testing::TestInfo* const NAMESPACE_GTEST_TEST_CLASS_NAME_(namespace_name, test_case_name, test_name)\
  ::test_info_ =\
    ::testing::internal::MakeAndRegisterTestInfo(\
        #namespace_name "." #test_case_name, #test_name, NULL, NULL, /* <-- Defines the test as "Namespace.Classname" */ \
        ::testing::internal::CodeLocation(__FILE__, __LINE__), \
        (parent_id), \
        parent_class::SetUpTestCase, \
        parent_class::TearDownTestCase, \
        new ::testing::internal::TestFactoryImpl<\
            NAMESPACE_GTEST_TEST_CLASS_NAME_(namespace_name, test_case_name, test_name)>);\
void NAMESPACE_GTEST_TEST_CLASS_NAME_(namespace_name, test_case_name, test_name)::TestBody()
#endif
// Simple macro
#define NAMESPACE_TEST(namespace_name, test_case_name, test_name) \
  NAMESPACE_GTEST_TEST_(namespace_name, test_case_name, test_name,\
    ::testing::Test, ::testing::internal::GetTestTypeId())

// Text Fixture
#define NAMESPACE_TEST_F(namespace_name, test_fixture, test_name)\
  NAMESPACE_GTEST_TEST_(namespace_name, test_fixture, test_name, test_fixture, \
    ::testing::internal::GetTypeId<test_fixture>())

// ---------------------------------------------------------------
// Variable Depth Namespace
// - Pass namespaces as trailing arguments to the Variadic Macro
// - VERY HIGHLY INSPIRED by https://stackoverflow.com/questions/1872220/is-it-possible-to-iterate-over-arguments-in-variadic-macros
// - Honorable mention for EXPAND https://stackoverflow.com/questions/5134523/msvc-doesnt-expand-va-args-correctly
// 
// - The class foo::bar::ClassName could be represented as
//   NAMESPACED_TEST(ClassName, TestName, foo, bar)
// 
// - Right now this supports a depth up to 5 sub namespaces (should be more than enough?)
// ---------------------------------------------------------------

// Make a FOREACH macro
#define G_FE_0(WHAT)
#define G_FE_1(WHAT, X) WHAT(X) 
#define G_FE_2(WHAT, X, ...) WHAT(X)G_FE_1(WHAT, __VA_ARGS__)
#define G_FE_3(WHAT, X, ...) WHAT(X)G_FE_2(WHAT, __VA_ARGS__)
#define G_FE_4(WHAT, X, ...) WHAT(X)G_FE_3(WHAT, __VA_ARGS__)
#define G_FE_5(WHAT, X, ...) WHAT(X)G_FE_4(WHAT, __VA_ARGS__)
//... repeat as needed

// Macro Expansion Fix for Visual Studio
#if _MSC_VER
#define G_FOREACH_EXPAND( X ) X
#else
#define G_FOREACH_EXPAND(X) X
#endif

// FOREACH
#define G_GET_MACRO(_0,_1,_2,_3,_4,_5,NAME,...) NAME 
#define G_FOR_EACH(action,...) \
  G_FOREACH_EXPAND(G_GET_MACRO(_0,__VA_ARGS__,G_FE_5,G_FE_4,G_FE_3,G_FE_2,G_FE_1,G_FE_0)(action,__VA_ARGS__))

// Some actions
#define G_QUALIFIER(X) X::
#define G_NS_QUALIFIER(X) ":"#X
#define G_NS_NAME(X) #X"."
#define G_NS_OPEN(X) namespace X {
#define G_NS_CLOSE(X) };
// Helper function
#define G_QUALIFIED(NAME,...) G_FOR_EACH(G_QUALIFIER,__VA_ARGS__)NAME

// Variadic Namespace Depth
#define NAMESPACED_GTEST_TEST_(test_case_name, test_name, parent_class, parent_id, ...)\
G_FOR_EACH(G_NS_OPEN,__VA_ARGS__) /* <-- define namespaces */ \
\
class GTEST_TEST_CLASS_NAME_(test_case_name, test_name) : public parent_class {\
 public:\
  GTEST_TEST_CLASS_NAME_(test_case_name, test_name)() {}\
 private:\
  virtual void TestBody();\
  static ::testing::TestInfo* const test_info_ GTEST_ATTRIBUTE_UNUSED_;\
  GTEST_DISALLOW_COPY_AND_ASSIGN_(\
      GTEST_TEST_CLASS_NAME_(test_case_name, test_name));\
};\
\
::testing::TestInfo* const GTEST_TEST_CLASS_NAME_(test_case_name, test_name)\
  ::test_info_ = ::testing::internal::MakeAndRegisterTestInfo(\
        G_FOR_EACH(G_NS_NAME,__VA_ARGS__)#test_case_name, #test_name, NULL, NULL, /* <-- Defines the test as "Namespace.Classname" */ \
        ::testing::internal::CodeLocation(__FILE__, __LINE__), \
        (parent_id), \
        parent_class::SetUpTestCase, \
        parent_class::TearDownTestCase, \
        new ::testing::internal::TestFactoryImpl<\
            GTEST_TEST_CLASS_NAME_(test_case_name, test_name)>);\
\
G_FOR_EACH(G_NS_CLOSE,__VA_ARGS__) /* <-- close namespaces */\
\
void G_QUALIFIED(GTEST_TEST_CLASS_NAME_(test_case_name, test_name), __VA_ARGS__)::TestBody()

// Simple Macro
#define NAMESPACED_TEST(test_case_name, test_name, ...) \
  NAMESPACED_GTEST_TEST_(test_case_name, test_name,\
    ::testing::Test, ::testing::internal::GetTestTypeId(),\
    __VA_ARGS__)

// Text Fixture
#define NAMESPACED_TEST_F(test_fixture, test_name, ...)\
  NAMESPACED_GTEST_TEST_(test_fixture, test_name, test_fixture, \
    ::testing::internal::GetTypeId<test_fixture>(), __VA_ARGS__)

#endif  // NAMESPACE_GTEST_INCLUDE_GTEST_GTEST_H_
