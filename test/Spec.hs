{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Eval
import Expr
import Parser
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)

main :: IO ()
main = hspec $ do
    describe "Basic Arithmetic" $ do
        it "evaluates addition" $ do
            env <- primitiveEnv
            result <- runExceptT $ eval env (List [Atom "+", Number 2, Number 3])
            result `shouldBe` Right (Number 5)

        it "evaluates subtraction" $ do
            env <- primitiveEnv
            result <- runExceptT $ eval env (List [Atom "-", Number 10, Number 4])
            result `shouldBe` Right (Number 6)

        it "evaluates multiplication" $ do
            env <- primitiveEnv
            result <- runExceptT $ eval env (List [Atom "*", Number 3, Number 4])
            result `shouldBe` Right (Number 12)

        it "evaluates division" $ do
            env <- primitiveEnv
            result <- runExceptT $ eval env (List [Atom "/", Number 10, Number 2])
            result `shouldBe` Right (Number 5)

    describe "Boolean Logic" $ do
        it "evaluates not" $ do
            env <- primitiveEnv
            result <- runExceptT $ eval env (List [Atom "not", Bool True])
            result `shouldBe` Right (Bool False)

        it "evaluates and" $ do
            env <- primitiveEnv
            result <- runExceptT $ eval env (List [Atom "and", Bool True, Bool False])
            result `shouldBe` Right (Bool False)

        it "evaluates or" $ do
            env <- primitiveEnv
            result <- runExceptT $ eval env (List [Atom "or", Bool False, Bool True])
            result `shouldBe` Right (Bool True)

    describe "Let Bindings" $ do
        it "evaluates single let binding" $ do
            env <- primitiveEnv
            result <- runExceptT $ eval env (List [Atom "let", List [List [Atom "x", Number 5]], Atom "x"])
            result `shouldBe` Right (Number 5)

        it "evaluates multiple let bindings" $ do
            env <- primitiveEnv
            result <- runExceptT $ eval env (List
                [Atom "let",
                    List [List [Atom "x", Number 5], List [Atom "y", Number 10]],
                    List [Atom "+", Atom "x", Atom "y"]
                ])
            result `shouldBe` Right (Number 15)

    describe "Functions and Recursion" $ do
        it "evaluates recursive factorial" $ do
            env <- primitiveEnv
            _ <- runExceptT $ eval env (List [
                Atom "define",
                List [Atom "factorial", Atom "n"],
                List [
                    Atom "if",
                    List [Atom "<=", Atom "n", Number 1],
                    Number 1,
                    List [Atom "*", Atom "n", List [Atom "factorial", List [Atom "-", Atom "n", Number 1]]]
                ]])
            result <- runExceptT $ eval env (List [Atom "factorial", Number 5])
            result `shouldBe` Right (Number 120)
