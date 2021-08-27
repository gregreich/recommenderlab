## always recommends the top-N popular items (without known items)

BIN_POPULAR <- function(data, parameter = NULL) {

  cnt <- colCounts(data)
  pop <- rank(cnt-max(cnt), ties.method = "random")/length(cnt) ## pop is proportional to item count

  ratings <- new("realRatingMatrix",
    data = dropNA(t(pop)))

  topN <- new("topNList",
    items = list(order(cnt, decreasing=TRUE)),
    itemLabels = colnames(data),
    n= ncol(data))

  model <- list(
    topN = topN,
    ratings = ratings,
    labels = colnames(data)
  )

  predict <- function(model, newdata, n=10,
    data=NULL,  type=c("topNList", "ratings", "ratingMatrix"),...) {

    type <- match.arg(type)

    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata,]
    }

    ratings_pop <- model$ratings
    colnames(ratings_pop) <- model$labels
    
    for(i in 1:nrow(newdata)) {
      
      data_user <- newdata[i,]
      rownames(ratings_pop) <- rownames(data_user)
      
      ratings_user <- as(returnRatings(ratings_pop, data_user, type, n), "dgCMatrix")
    
      if (i==1){
        ratings <- ratings_user
      }else{
        ratings <- rbind(ratings, ratings_user)
      }
    }
    
    ratings <- as(ratings, "realRatingMatrix")
    rownames(ratings) <- rownames(newdata)
    colnames(ratings) <- model$labels
    
    if(type=="topNList")
      ratings <- getTopNLists(ratings, n = n)
    
    ratings
    
  }

  ## construct recommender object
  new("Recommender", method = "POPULAR", dataType = class(data),
    ntrain = nrow(data), model = model, predict = predict)
}

## register recommender
recommenderRegistry$set_entry(
  method="POPULAR", dataType = "binaryRatingMatrix", fun=BIN_POPULAR,
  description="Recommender based on item popularity."
)

.REAL_POPULAR_params <- list(
    normalize="center",
    aggregationRatings=colMeans,
    aggregationPopularity=colSums
)


## always recommends the top-N popular items (without known items)
REAL_POPULAR <- function(data, parameter = NULL) {

  p <- getParameters(.REAL_POPULAR_params, parameter)

  data <- normalize(data, method=p$normalize)

  ratings <- new("realRatingMatrix",
    data = dropNA(t(p$aggregationRatings(data))),
    normalize = data@normalize)

  topN <- new("topNList",
    items = list(order(p$aggregationPopularity(data), decreasing=TRUE)),
    itemLabels = colnames(data),
    n= ncol(data))

  model <- c(list(
    topN = topN,
    ratings = ratings
  ), p)

  predict <- function(model, newdata, n=10,
    data=NULL, type=c("topNList", "ratings", "ratingMatrix"), ...) {

    type <- match.arg(type)

    if(is.numeric(newdata)) {
      if(is.null(data) || !is(data, "ratingMatrix"))
        stop("If newdata is a user id then data needes to be the training dataset.")
      newdata <- data[newdata,]
    }

    if(ncol(newdata) != ncol(model$ratings)) stop("number of items in newdata does not match model.")

    ### create denormalized data for each new user
    newdata <- normalize(newdata, method = model$normalize)
    ratings <- model$ratings[rep(1L, nrow(newdata)),]
    ratings@normalize <- getNormalize(newdata)
    ratings <- denormalize(ratings, getNormalize(newdata))

    rownames(ratings) <- rownames(newdata)

    ### this is because we use populary and not average rating here!
    if(type=="topNList") {
      topN <- model$topN
      topN@items <- structure(
        replicate(nrow(newdata), topN@items, simplify = TRUE),
        names = rownames(newdata))

      topN@ratings <- structure(lapply(1:length(topN@items),
        function(i) as(as(ratings[i, topN@items[[i]]], "matrix"), "vector")),
        names = rownames(newdata))

      topN <- removeKnownItems(topN, newdata)
      topN <- bestN(topN, n)
      return(topN)
    }


    returnRatings(ratings, newdata, type, n)
  }

  ## construct recommender object
  new("Recommender", method = "POPULAR", dataType = class(data),
    ntrain = nrow(data), model = model, predict = predict)
}

## register recommender
recommenderRegistry$set_entry(
  method="POPULAR", dataType = "realRatingMatrix", fun=REAL_POPULAR,
  description="Recommender based on item popularity.",
  parameters=.REAL_POPULAR_params)
