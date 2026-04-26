#zad 1
komb <- combn(1:8, 5)

X <- apply(komb, 2, max)

tab <- table(X) #zlicza ile razy występuje każda wartość
tab

px <- tab / sum(tab) #częstość/wszystkie

px

barplot(px, main="Rozkład P(X=x)", xlab="x", ylab="P(X=x)", col=rgb(0.8,0.1,0.1,0.6))

Fx_emp <- ecdf(X)
plot(Fx_emp, main="Dystrybuanta empiryczna")

#zad 2
# dnorm(x, mean = 0, sd = 1, log = FALSE)
# pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
# qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
dnorm(3, mean=2, sd=5) #wartość funkcji gęstości prawdopodobieństwa
pnorm(7, mean=2, sd=5) #dystrybuanta, P(X ≤ 7)
qnorm(0.8, mean=2, sd=5) # kwantyl 4/5 = 80%, wartość x taka, że P(X ≤ x) = 0.8

# Taka sama konwencja - d dla gęstości, p dla dystrybuanty, i q dla kwantyli

dt(3, df=5)
pt(7, df=5)
qt(0.8, df=5)

#zad3
# Rozkład 0, 1 więc nie musimy podawać parametrów
# 1. Dystrybuanta
pnorm(1.5)

# 2. Różnica dystrybuant
pnorm(2.5) - pnorm(-2.5)

# N(2,1) 
pnorm(1.5, mean=2, sd=1)

pnorm(2.5, 2, 1) - pnorm(-2.5, 2, 1)

#zad4
# punif(0.5, min = 0, max = 1) rozkład jednostajny
punif(1, -3, 3)
punif(0.5, -3, 3) - punif(-0.5, -3, 3)
1 - punif(-1, -3, 3)


# pexp(x_dexp, rate = 5)   Rozkład wykładniczy
pexp(1, 1.5)
pexp(1.5, 1.5) - pexp(0.5, 1.5)
1 - pexp(4, 1.5)

#zad5
set.seed(42)
x <- rnorm(1000) # losujemy 100 wartości w celu symulacji

mean(abs(x) < 1) # mniej niż 68
mean(abs(x) < 2) #
mean(abs(x) < 3)

hist(x, probability=TRUE,
     main="Reguła 3 sigma",
     xlab="x",
     col=rgb(0.8,0.1,0.1,0.6))

abline(v=c(-1,1))
abline(v=c(-2,2))
abline(v=c(-3,3))

# Rozkład teoretyczny
pnorm(1) - pnorm(-1)
pnorm(2) - pnorm(-2)
pnorm(3) - pnorm(-3)


#zad6
set.seed(123)
x_exp <- rexp(100, rate=1.5) # Wybrano rozkłąd wykładniczy

plot(ecdf(x_exp), main="Dystrybuanta wykładnicza (empiryczna)",
     xlab="x", ylab="F(x)", col=rgb(0.8,0.1,0.1,0.6))

curve(pexp(x, rate=1.5), add=TRUE, lty=2) 
