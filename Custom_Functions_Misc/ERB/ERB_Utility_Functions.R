list(
createSGPLookupTable = 
function(state,
    sgp.config,
    csem_table,
    content_area_grade_modality_difficulty_combinations
) {

    ### Parameters
    CONTENT_AREAS_ORIGINAL <- c('ALGEBRA', 'AUDITORY_COMPREHENSION', 'MATHEMATICS', 'QUANTITATIVE_REASONING', 'READING_COMPREHENSION', 'SCIENCE', 'VERBAL_REASONING', 'VOCABULARY', 'WORD_ANALYSIS', 'WRITING_CONCEPTS_&_SKILLS', 'WRITING_MECHANICS')


    ### Utility function
    convert_to_long <- function(wide.dt, content_area_sequence, year_sequence, grade_sequence) {
        tmp.dt <- rbind(
                    data.table(VALID_CASE="VALID_CASE", CONTENT_AREA=content_area_sequence[1], YEAR=year_sequence[1], GRADE=grade_sequence[1], ID=paste(seq(dim(wide.dt)[1]), paste(grade_sequence, collapse=""), sep="_"), SCALE_SCORE=as.numeric(wide.dt[['V1']])),
                    data.table(VALID_CASE="VALID_CASE", CONTENT_AREA=content_area_sequence[2], YEAR=year_sequence[2], GRADE=grade_sequence[2], ID=paste(seq(dim(wide.dt)[1]), paste(grade_sequence, collapse=""), sep="_"), SCALE_SCORE=as.numeric(wide.dt[['V2']]))
                )
        return(tmp.dt)
    }

    permutation_subsets <- function(tmp.dt.combos.list) {
        tmp.list <- tmp.list.wo.grade <- list()
        for (grade.iter in names(tmp.dt.combos.list)) {
            tmp.grades <- unlist(strsplit(grade.iter, "_"))
            tmp.grid <- as.data.table(expand.grid(which(tmp.dt.combos.list[[grade.iter]]$GRADE==tmp.grades[1]), which(tmp.dt.combos.list[[grade.iter]]$GRADE==tmp.grades[2])))
            for (row.iter in seq(nrow(tmp.grid))) {
                tmp.list[[grade.iter]][[row.iter]] <- data.table(tmp.dt.combos.list[[grade.iter]][as.numeric(tmp.grid[row.iter])][,YEAR:=year_sequence], key=c("CONTENT_AREA_CSEM_LABEL", "YEAR"))
                tmp.list.wo.grade[[grade.iter]][[row.iter]] <- data.table(tmp.dt.combos.list[[grade.iter]][as.numeric(tmp.grid[row.iter])][,YEAR:=year_sequence][,GRADE:=NULL], key=c("CONTENT_AREA_CSEM_LABEL", "YEAR"))
            }
        }
        return(list(Permutation_Tables=unlist(tmp.list, recursive=FALSE), Unique_Permutation_Tables_WO_Grade=unique(unlist(tmp.list.wo.grade, recursive=FALSE))))
    }

    ### Create table with all scale score permutations 
    tmp.dt.list <- list()
    tmp.dt.combos.list <- list()
    content_area_sequence <- tail(sgp.config[[1]][['sgp.content.areas']], 2)
    content_area_csem_label_sequence <- as.character(sapply(content_area_sequence, function(y) names(unlist(sapply(CONTENT_AREAS_ORIGINAL, function(x) grep(x, y))))))
    year_sequence <- tail(sgp.config[[1]][['sgp.panel.years']], 2)
    for (grade_sequence.iter in seq_along(sgp.config[[1]][['sgp.grade.sequences']])) {
        grade_sequence <- tail(sgp.config[[1]][['sgp.grade.sequences']][grade_sequence.iter][[1]], 2)
        prior_range <- SGP::SGPstateData[[state]][['Achievement']][['Knots_Boundaries']][[content_area_sequence[1]]][[paste("loss.hoss", grade_sequence[1], sep="_")]]
        current_range <- SGP::SGPstateData[[state]][['Achievement']][['Knots_Boundaries']][[content_area_sequence[2]]][[paste("loss.hoss", grade_sequence[2], sep="_")]]
        tmp.dt.list[[grade_sequence.iter]] <- convert_to_long(data.table::CJ(seq(prior_range[1], prior_range[2]), seq(current_range[1], current_range[2])), content_area_sequence, year_sequence, grade_sequence)
        tmp.dt.combos.list[[paste(grade_sequence, collapse="_")]] <- content_area_grade_modality_difficulty_combinations[GRADE %in% grade_sequence & CONTENT_AREA_CSEM_LABEL %in% content_area_csem_label_sequence]
    }
    tmp.dt <- rbindlist(tmp.dt.list)

    ### Merge CSEMs
    tmp.list <- list()
    tmp.dt[,CONTENT_AREA_CSEM_LABEL:=as.factor(CONTENT_AREA)]
    new.levels <- as.character(sapply(levels(tmp.dt[['CONTENT_AREA_CSEM_LABEL']]), function(y) names(unlist(sapply(CONTENT_AREAS_ORIGINAL, function(x) grep(x, y))))))
    setattr(tmp.dt[['CONTENT_AREA_CSEM_LABEL']], "levels", new.levels)
    for (levels.iter in new.levels) {
        tmp.list[[levels.iter]] <- tmp.dt[content_area_grade_modality_difficulty_combinations[CONTENT_AREA_CSEM_LABEL==levels.iter], on=c("CONTENT_AREA_CSEM_LABEL", "GRADE"), allow.cartesian=TRUE]
    }
    if (length(tmp.list)==1L) tmp.all <- tmp.list[[1L]] else tmp.all <- rbindlist(tmp.list)
    tmp.final <- csem_table[tmp.all, on=c("CONTENT_AREA_CSEM_LABEL", "GRADE", "MODALITY", "DIFFICULTY", "SCALE_SCORE")]

    ### Create permutation subsets
    tmp.data.subsets.list <- list()
    tmp.permutation.subsets <- permutation_subsets(tmp.dt.combos.list)
    for (permutations.iter in seq(length(tmp.permutation.subsets[["Unique_Permutation_Tables_WO_Grade"]]))) {
            tmp.index <- which(sapply(tmp.permutation.subsets[['Permutation_Tables']], function(x) identical(x[,c("CONTENT_AREA_CSEM_LABEL", "MODALITY", "DIFFICULTY", "YEAR"), with=FALSE], tmp.permutation.subsets[['Unique_Permutation_Tables_WO_Grade']][[permutations.iter]])))
            subset.list <- rbindlist(tmp.permutation.subsets[['Permutation_Tables']][tmp.index])
            tmp.label <- paste(paste(tmp.permutation.subsets[['Unique_Permutation_Tables_WO_Grade']][[permutations.iter]]$MODALITY, tmp.permutation.subsets[['Unique_Permutation_Tables_WO_Grade']][[permutations.iter]]$DIFFICULTY, sep="/"), collapse="/")
            tmp.data.subsets.list[[tmp.label]] <- tmp.final[subset.list, on=c("CONTENT_AREA_CSEM_LABEL", "GRADE", "MODALITY", "DIFFICULTY", "YEAR")][, c("VALID_CASE", "CONTENT_AREA", "YEAR", "ID", "GRADE", "SCALE_SCORE", "SCALE_SCORE_CSEM", "MODALITY", "DIFFICULTY", "CONTENT_AREA_CSEM_LABEL"), with=FALSE]
    }
    return(tmp.data.subsets.list)
}, ### END createSGPLookupTable


getCSEMLookup = 
function(SCALE_SCORE,
        CSEM
) {
        tmp.approx.fun <- approxfun(SCALE_SCORE, CSEM)
        tmp.sequence.scale.scores <- seq(min(SCALE_SCORE, na.rm=TRUE), max(SCALE_SCORE, na.rm=TRUE))

        my.tmp <- data.table(
            SCALE_SCORE=tmp.sequence.scale.scores,
            SCALE_SCORE_CSEM=tmp.approx.fun(tmp.sequence.scale.scores)
        )

        return(my.tmp)
} ### END get.csem.lookup 
) ### END list
