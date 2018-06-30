
a <- array(1:24, dim = 2:4)
n.within <- 2L
n.along <- 3L
n.between <- 4L
s.within <- seq_len(n.within) - 1L
s.along <- seq_len(n.along) - 1L
s.between <- seq_len(n.between) - 1L
i.within <- rep(s.within, times = n.along * n.between)
i.along <- rep(rep(s.along, each = n.within), times = n.between)
i.between <- rep(s.between, each = n.within * n.along)
i.within + n.within * i.along + n.within * n.along * i.between


a <- array(1:24, dim = 2:4)
n.within <- 1L
n.along <- 2L
n.between <- 12L
s.within <- seq_len(n.within) - 1L
s.along <- seq_len(n.along) - 1L
s.between <- seq_len(n.between) - 1L
i.within <- rep(s.within, times = n.along * n.between)
i.along <- rep(rep(s.along, each = n.within), times = n.between)
i.between <- rep(s.between, each = n.within * n.along)
i.within + n.within * i.along + n.within * n.along * i.between


a <- array(1:24, dim = 2:4)
n.within <- 6L
n.along <- 4L
n.between <- 1L
s.within <- seq_len(n.within) - 1L
s.along <- seq_len(n.along) - 1L
s.between <- seq_len(n.between) - 1L
i.within <- rep(s.within, times = n.along * n.between)
i.along <- rep(rep(s.along, each = n.within), times = n.between)
i.between <- rep(s.between, each = n.within * n.along)
i.within + n.within * i.along + n.within * n.along * i.between





           
x <- Counts(array(1:24,
                  dim = 2:4,
                  dimnames = list(sex = c("f", "m"),
                                  age = c("0-4", "5-9", "10-14"),
                                  time = c(2000, 2005, 2010, 2015))))
ans <- rotateAgeTime(x, to = "tc")

rotateAgeTime(ans, to = "at")


