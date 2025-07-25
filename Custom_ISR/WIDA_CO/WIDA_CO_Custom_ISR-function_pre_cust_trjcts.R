`make.custom.isr` <-
    function() {
        ## Start pdf device
        report.width <- custom.isr$report.width
        report.height <- custom.isr$report.height

        pdf(file.path(path.to.pdfs, file_name),
            width = report.width, height = report.height, version = "1.4", onefile = TRUE
        )

        ##  Push full report layout viewport
        pushViewport(custom.isr$Grid_Objects$report.vp)
        ##  Push/pop content area report blocks
        for (vp in seq_along(content_areas)) {
            tmp_student_data <- as.data.frame(tmp_grade_data[ID == n & CONTENT_AREA == content_areas[vp]])
            if (tmp_student_data[[paste("ACHIEVEMENT_LEVEL", last.year, sep = ".")]] == "L6") {
                tmp_student_data[, grep("PROJ_YEAR", names(tmp_student_data))] <- NA
            }
            pushViewport(eval(parse(text = paste("custom.isr$Grid_Objects$content_area_", vp, ".vp", sep = ""))))
            studentGrowthPlot(
                Scale_Scores = as.numeric(subset(tmp_student_data, select = paste("SCALE_SCORE", rev(sgPlot.years), sep = "."))),
                Plotting_Scale_Scores = as.numeric(subset(tmp_student_data, select = paste("TRANSFORMED_SCALE_SCORE", rev(sgPlot.years), sep = "."))),
                Achievement_Levels = as.character(unlist(subset(tmp_student_data, select = paste("ACHIEVEMENT_LEVEL", rev(sgPlot.years), sep = ".")))),
                SGP = as.numeric(subset(tmp_student_data, select = paste(my.sgp, rev(sgPlot.years), sep = "."))),
                SGP_Levels = as.character(unlist(subset(tmp_student_data, select = paste(my.sgp.level, rev(sgPlot.years), sep = ".")))),
                Grades = as.character(subset(tmp_student_data, select = paste("GRADE", rev(sgPlot.years), sep = "."))),
                Content_Areas = as.character(subset(tmp_student_data, select = paste("CONTENT_AREA_LABELS", rev(sgPlot.years), sep = "."))),
                Cuts = list(
                    NY1 = as.numeric(subset(tmp_student_data, select = setdiff(intersect(
                        grep(trajectory.cuts, names(tmp_student_data)),
                        grep("YEAR_1", names(tmp_student_data))
                    ), grep("YEAR_1_CURRENT_TRANSFORMED", names(tmp_student_data))))),
                    NY2 = as.numeric(subset(tmp_student_data, select = setdiff(intersect(
                        grep(trajectory.cuts, names(tmp_student_data)),
                        grep("YEAR_2", names(tmp_student_data))
                    ), grep("YEAR_2_CURRENT_TRANSFORMED", names(tmp_student_data))))),
                    NY3 = as.numeric(subset(tmp_student_data, select = setdiff(intersect(
                        grep(trajectory.cuts, names(tmp_student_data)),
                        grep("YEAR_3", names(tmp_student_data))
                    ), grep("YEAR_3_CURRENT_TRANSFORMED", names(tmp_student_data)))))
                ),
                Plotting_Cuts = list(
                    NY1 = as.numeric(subset(tmp_student_data, select = intersect(grep(trajectory.cuts, names(tmp_student_data)), grep("YEAR_1_CURRENT_TRANSFORMED", names(tmp_student_data))))),
                    NY2 = as.numeric(subset(tmp_student_data, select = intersect(grep(trajectory.cuts, names(tmp_student_data)), grep("YEAR_2_CURRENT_TRANSFORMED", names(tmp_student_data))))),
                    NY3 = as.numeric(subset(tmp_student_data, select = intersect(grep(trajectory.cuts, names(tmp_student_data)), grep("YEAR_3_CURRENT_TRANSFORMED", names(tmp_student_data)))))
                ),
                SGP_Targets = list(
                    CUKU = tmp_student_data[[paste(paste(my.sgp.target.label[1], my.sgp.target.label[2], sep = "_"), last.year, sep = ".")]],
                    CUKU_Current = tmp_student_data[[paste(paste(my.sgp.target.label[1], my.sgp.target.label[2], "CURRENT", sep = "_"), last.year, sep = ".")]],
                    MUSU = tmp_student_data[[paste(paste(my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], sep = "_"), last.year, sep = ".")]],
                    MUSU_Current = tmp_student_data[[paste(paste(my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "CURRENT", sep = "_"), last.year, sep = ".")]]
                ),
                SGP_Scale_Score_Targets = list(
                    CUKU = list(
                        NY1 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_1", sep = "_")]]),
                        NY2 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_2", sep = "_")]]),
                        NY3 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_3", sep = "_")]])
                    ),
                    MUSU = list(
                        NY1 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_1", sep = "_")]]),
                        NY2 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_2", sep = "_")]]),
                        NY3 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_3", sep = "_")]])
                    ),
                    CUKU_Current = list(
                        NY1 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_1_CURRENT", sep = "_")]]),
                        NY2 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_2_CURRENT", sep = "_")]]),
                        NY3 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_3_CURRENT", sep = "_")]])
                    ),
                    MUSU_Current = list(
                        NY1 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_1_CURRENT", sep = "_")]]),
                        NY2 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_2_CURRENT", sep = "_")]]),
                        NY3 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_3_CURRENT", sep = "_")]])
                    )
                ),
                Plotting_SGP_Scale_Score_Targets = list(
                    CUKU = list(
                        NY1 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_1_TRANSFORMED", sep = "_")]]),
                        NY2 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_2_TRANSFORMED", sep = "_")]]),
                        NY3 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_3_TRANSFORMED", sep = "_")]])
                    ),
                    MUSU = list(
                        NY1 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_1_TRANSFORMED", sep = "_")]]),
                        NY2 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_2_TRANSFORMED", sep = "_")]]),
                        NY3 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_3_TRANSFORMED", sep = "_")]])
                    ),
                    CUKU_Current = list(
                        NY1 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_1_CURRENT_TRANSFORMED", sep = "_")]]),
                        NY2 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_2_CURRENT_TRANSFORMED", sep = "_")]]),
                        NY3 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_3_CURRENT_TRANSFORMED", sep = "_")]])
                    ),
                    MUSU_Current = list(
                        NY1 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_1_CURRENT_TRANSFORMED", sep = "_")]]),
                        NY2 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_2_CURRENT_TRANSFORMED", sep = "_")]]),
                        NY3 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_3_CURRENT_TRANSFORMED", sep = "_")]])
                    )
                ),
                Cutscores = sgPlot.cutscores[[content_areas[vp]]],
                Years = rev(sgPlot.years),
                Report_Parameters = list(
                    Current_Year = last.year, Content_Area = content_areas[vp], State = "WIDA_CO", SGP_Targets = sgPlot.sgp.targets,
                    # Content_Area_Title=tmp_student_data[[paste("CONTENT_AREA_LABELS", last.year, sep=".")]], Configuration= list(Font_Size="Small_1")))
                    Content_Area_Title = tmp_student_data[[paste("CONTENT_AREA_LABELS", last.year, sep = ".")]], Configuration = list(Zero_to_K = TRUE, Font_Size = list(
                        title.ca.size = 1.6, legend.size = 0.6, bottom.right.vp.size = 1.2, bottom.left.vp.size = 0.6
                    ))
                )
            )

            popViewport()
        } ## END loop over content_areas


        ## Top Border/Banner
        pushViewport(custom.isr$Grid_Objects$top.school.name.vp)
        grid.rect(gp = gpar(fill = "#232c67", col = "#232c67"))
        grid.text(x = 0.05, y = 0.5, tmp_school_name, gp = gpar(fontface = "bold", fontfamily = "Helvetica-Narrow", col = "white", cex = 1.2), just = "left", default.units = "native")
        popViewport()

        pushViewport(custom.isr$Grid_Objects$top.student.name.vp)
        grid.rect(gp = gpar(fill = "#235e39", col = "#235e39"))
        grid.text(
            x = 0.075, y = 0.5, paste(FIRST_NAME, " ", LAST_NAME, sep = ""),
            gp = gpar(fontface = "bold", fontfamily = "Helvetica-Narrow", col = "white", cex = 1.1), just = "left", default.units = "native"
        )
        popViewport()
        pushViewport(custom.isr$Grid_Objects$top.student.id.vp)
        grid.rect(gp = gpar(fill = "#00953a", col = "#00953a"))
        grid.text(
            x = 0.09, y = 0.5, paste("(", student_number, ")", sep = ""),
            gp = gpar(fontface = "bold", fontfamily = "Helvetica-Narrow", col = "white", cex = 1.1), just = "left", default.units = "native"
        )
        popViewport()

        pushViewport(custom.isr$Grid_Objects$top.border.cde.vp)
        grid.raster(custom.isr$Report_Logo)
        popViewport()

        ##  Report Title
        pushViewport(custom.isr$Grid_Objects$report_title.vp)
        grid.text(paste(last.year,"English Language Proficiency and Student Growth Report"),
            gp = gpar(fontface = "bold", fontfamily = "Helvetica", col = "black", cex = 1.125), just = "top", default.units = "native"
        ) # Helvetica-Narrow
        popViewport()

        ##  Color block 1
        pushViewport(custom.isr$Grid_Objects$color_block_1.vp)
        grid.rect(gp = gpar(fill = "#2c3384", col = "#2c3384"))
        popViewport()

        ##  Report Text
        # pushViewport(custom.isr$Grid_Objects$report_text.vp)
        # grid.raster(custom.isr$Report_Text_ENGLISH)
        # popViewport()
        pushViewport(custom.isr$Grid_Objects$report_text.vp)
        grid.text(
            x = 0.01, y = 0.5, # x = 0.025,
            label = custom.isr$Report_Text_ENGLISH,
            gp = gpar(fontfamily = "Helvetica-Narrow", col = "black", cex = 0.825),
            just = "left", default.units = "native"
        )
        popViewport()

        ##  Color block 2
        pushViewport(custom.isr$Grid_Objects$color_block_2.vp)
        grid.rect(gp = gpar(fill = "#2c3384", col = "#2c3384"))
        popViewport()

        pushViewport(custom.isr$Grid_Objects$report.student.name.vp)
        grid.rect(gp = gpar(fill = "#235e39", col = "#235e39"))
        grid.text(
            x = 0.05, y = 0.5, paste(FIRST_NAME, " ", LAST_NAME, sep = ""),
            gp = gpar(fontface = "bold", fontfamily = "Helvetica-Narrow", col = "white", cex = 1.15), just = "left", default.units = "native"
        )
        popViewport()
        # pushViewport(custom.isr$Grid_Objects$report.school.name.vp)
        # grid.rect(gp=gpar(fill="#339933", col="#339933"))
        # grid.text(x= 0.125, y=0.5, tmp_school_name, gp=gpar(fontface="bold", fontfamily="Helvetica-Narrow", col="white", cex=1.25), just="left", default.units="native")
        # popViewport()

        # pushViewport(custom.isr$Grid_Objects$left.border.vp)
        # grid.rect(gp = gpar(fill = "#2c3384", col = "#2c3384"))
        # grid.text("2024 English Language Proficiency and Student Growth Report",
        #     gp = gpar(fontface = "bold", fontfamily = "Helvetica-Narrow", col = "white", cex = 2), rot = 270, just = "center", default.units = "native"
        # )
        # popViewport()

        ## Bottom Legend
        pushViewport(custom.isr$Grid_Objects$bottom.border.vp)
        # grid.text(x=0.02, y=0.65, paste0("For more information please visit www.cde.state.co.us/accountability/englishlanguageproficiencygrowth or contact ", tmp.organization$Contact, "."), gp=gpar(cex=0.75, col="black"), default.units="native", just=c("left", "top"))
        copyright.text <- paste0("Cooperatively developed by the ", tmp.organization$Name, " (", tmp.organization$Abbreviation, ") & the Center for Assessment, Inc.")
        grid.text(
            x = 0.02, y = 0.50, paste0(copyright.text, " Distributed by ", tmp.organization$Abbreviation, "."), # y=0.30
            gp = gpar(cex = 0.85, col = "black", fontface = "bold", fontfamily = "Helvetica-Narrow"), default.units = "native", just = c("left", "top")
        )
        popViewport()


        #######################
        ##  Spanish Version  ##
        #######################

        ##  See changes to "Report_Parameters" argument in call to studentGrowthPlot
        grid.newpage()

        ##  Push full report layout viewport
        pushViewport(custom.isr$Grid_Objects$report.vp)

        ##  Push/pop content area report blocks
        for (vp in seq_along(content_areas)) {
            tmp_student_data <- as.data.frame(tmp_grade_data[ID == n & CONTENT_AREA == content_areas[vp]])
            if (tmp_student_data[[paste("ACHIEVEMENT_LEVEL", last.year, sep = ".")]] == "L6") {
                tmp_student_data[, grep("PROJ_YEAR", names(tmp_student_data))] <- NA
            }
            pushViewport(eval(parse(text = paste("custom.isr$Grid_Objects$content_area_", vp, ".vp", sep = ""))))
            studentGrowthPlot(
                Scale_Scores = as.numeric(subset(tmp_student_data, select = paste("SCALE_SCORE", rev(sgPlot.years), sep = "."))),
                Plotting_Scale_Scores = as.numeric(subset(tmp_student_data, select = paste("TRANSFORMED_SCALE_SCORE", rev(sgPlot.years), sep = "."))),
                Achievement_Levels = as.character(unlist(subset(tmp_student_data, select = paste("ACHIEVEMENT_LEVEL", rev(sgPlot.years), sep = ".")))),
                SGP = as.numeric(subset(tmp_student_data, select = paste(my.sgp, rev(sgPlot.years), sep = "."))),
                SGP_Levels = as.character(unlist(subset(tmp_student_data, select = paste(my.sgp.level, rev(sgPlot.years), sep = ".")))),
                Grades = as.character(subset(tmp_student_data, select = paste("GRADE", rev(sgPlot.years), sep = "."))),
                Content_Areas = as.character(subset(tmp_student_data, select = paste("CONTENT_AREA_LABELS", rev(sgPlot.years), sep = "."))),
                Cuts = list(
                    NY1 = as.numeric(subset(tmp_student_data, select = setdiff(intersect(
                        grep(trajectory.cuts, names(tmp_student_data)),
                        grep("YEAR_1", names(tmp_student_data))
                    ), grep("YEAR_1_CURRENT_TRANSFORMED", names(tmp_student_data))))),
                    NY2 = as.numeric(subset(tmp_student_data, select = setdiff(intersect(
                        grep(trajectory.cuts, names(tmp_student_data)),
                        grep("YEAR_2", names(tmp_student_data))
                    ), grep("YEAR_2_CURRENT_TRANSFORMED", names(tmp_student_data))))),
                    NY3 = as.numeric(subset(tmp_student_data, select = setdiff(intersect(
                        grep(trajectory.cuts, names(tmp_student_data)),
                        grep("YEAR_3", names(tmp_student_data))
                    ), grep("YEAR_3_CURRENT_TRANSFORMED", names(tmp_student_data)))))
                ),
                Plotting_Cuts = list(
                    NY1 = as.numeric(subset(tmp_student_data, select = intersect(grep(trajectory.cuts, names(tmp_student_data)), grep("YEAR_1_CURRENT_TRANSFORMED", names(tmp_student_data))))),
                    NY2 = as.numeric(subset(tmp_student_data, select = intersect(grep(trajectory.cuts, names(tmp_student_data)), grep("YEAR_2_CURRENT_TRANSFORMED", names(tmp_student_data))))),
                    NY3 = as.numeric(subset(tmp_student_data, select = intersect(grep(trajectory.cuts, names(tmp_student_data)), grep("YEAR_3_CURRENT_TRANSFORMED", names(tmp_student_data)))))
                ),
                SGP_Targets = list(
                    CUKU = tmp_student_data[[paste(paste(my.sgp.target.label[1], my.sgp.target.label[2], sep = "_"), last.year, sep = ".")]],
                    CUKU_Current = tmp_student_data[[paste(paste(my.sgp.target.label[1], my.sgp.target.label[2], "CURRENT", sep = "_"), last.year, sep = ".")]],
                    MUSU = tmp_student_data[[paste(paste(my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], sep = "_"), last.year, sep = ".")]],
                    MUSU_Current = tmp_student_data[[paste(paste(my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "CURRENT", sep = "_"), last.year, sep = ".")]]
                ),
                SGP_Scale_Score_Targets = list(
                    CUKU = list(
                        NY1 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_1", sep = "_")]]),
                        NY2 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_2", sep = "_")]]),
                        NY3 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_3", sep = "_")]])
                    ),
                    MUSU = list(
                        NY1 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_1", sep = "_")]]),
                        NY2 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_2", sep = "_")]]),
                        NY3 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_3", sep = "_")]])
                    ),
                    CUKU_Current = list(
                        NY1 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_1_CURRENT", sep = "_")]]),
                        NY2 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_2_CURRENT", sep = "_")]]),
                        NY3 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_3_CURRENT", sep = "_")]])
                    ),
                    MUSU_Current = list(
                        NY1 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_1_CURRENT", sep = "_")]]),
                        NY2 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_2_CURRENT", sep = "_")]]),
                        NY3 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_3_CURRENT", sep = "_")]])
                    )
                ),
                Plotting_SGP_Scale_Score_Targets = list(
                    CUKU = list(
                        NY1 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_1_TRANSFORMED", sep = "_")]]),
                        NY2 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_2_TRANSFORMED", sep = "_")]]),
                        NY3 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_3_TRANSFORMED", sep = "_")]])
                    ),
                    MUSU = list(
                        NY1 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_1_TRANSFORMED", sep = "_")]]),
                        NY2 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_2_TRANSFORMED", sep = "_")]]),
                        NY3 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_3_TRANSFORMED", sep = "_")]])
                    ),
                    CUKU_Current = list(
                        NY1 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_1_CURRENT_TRANSFORMED", sep = "_")]]),
                        NY2 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_2_CURRENT_TRANSFORMED", sep = "_")]]),
                        NY3 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], my.sgp.target.label[2], "PROJ_YEAR_3_CURRENT_TRANSFORMED", sep = "_")]])
                    ),
                    MUSU_Current = list(
                        NY1 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_1_CURRENT_TRANSFORMED", sep = "_")]]),
                        NY2 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_2_CURRENT_TRANSFORMED", sep = "_")]]),
                        NY3 = as.numeric(tmp_student_data[[paste("SCALE_SCORE", my.sgp.target.label[1], "MOVE_UP_STAY_UP", my.sgp.target.label[2], "PROJ_YEAR_3_CURRENT_TRANSFORMED", sep = "_")]])
                    )
                ),
                Cutscores = sgPlot.cutscores[[content_areas[vp]]],
                Years = rev(sgPlot.years),
                Report_Parameters = list(
                    Current_Year = last.year, Content_Area = content_areas[vp], State = "WIDA_CO_SPANISH", SGP_Targets = sgPlot.sgp.targets,
                    # Content_Area_Title=tmp_student_data[[paste("CONTENT_AREA_LABELS", last.year, sep=".")]], Configuration= list(Language = "Spanish", Font_Size="Small_1")))
                    Content_Area_Title = tmp_student_data[[paste("CONTENT_AREA_LABELS", last.year, sep = ".")]], Configuration = list(Language = "Spanish", Zero_to_K = TRUE, Font_Size = list(
                        title.ca.size = 1.6, legend.size = 0.6, bottom.right.vp.size = 1.2, bottom.left.vp.size = 0.5
                    ))
                )
            )

            popViewport()
        } ## END loop over content_areas


        ## Top Border/Banner
        pushViewport(custom.isr$Grid_Objects$top.school.name.vp)
        grid.rect(gp = gpar(fill = "#232c67", col = "#232c67"))
        grid.text(x = 0.05, y = 0.5, tmp_school_name, gp = gpar(fontface = "bold", fontfamily = "Helvetica-Narrow", col = "white", cex = 1.2), just = "left", default.units = "native")
        popViewport()

        pushViewport(custom.isr$Grid_Objects$top.student.name.vp)
        grid.rect(gp = gpar(fill = "#235e39", col = "#235e39"))
        grid.text(
            x = 0.075, y = 0.5, paste(FIRST_NAME, " ", LAST_NAME, sep = ""),
            gp = gpar(fontface = "bold", fontfamily = "Helvetica-Narrow", col = "white", cex = 1.1), just = "left", default.units = "native"
        )
        popViewport()
        pushViewport(custom.isr$Grid_Objects$top.student.id.vp)
        grid.rect(gp = gpar(fill = "#00953a", col = "#00953a"))
        grid.text(
            x = 0.09, y = 0.5, paste("(", student_number, ")", sep = ""),
            gp = gpar(fontface = "bold", fontfamily = "Helvetica-Narrow", col = "white", cex = 1.1), just = "left", default.units = "native"
        )
        popViewport()

        pushViewport(custom.isr$Grid_Objects$top.border.cde.vp)
        grid.raster(custom.isr$Report_Logo)
        popViewport()

        ##  Report Title
        pushViewport(custom.isr$Grid_Objects$report_title.vp)
        grid.text(paste(last.year,"Reporte del Crecimiento Estudiantil en el Dominio del Idioma Ingl\u{E9}s"),
            gp = gpar(fontface = "bold", fontfamily = "Helvetica", col = "black", cex = 1.125), just = "top", default.units = "native"
        ) # Helvetica-Narrow
        popViewport()

        ##  Color block 1
        pushViewport(custom.isr$Grid_Objects$color_block_1.vp)
        grid.rect(gp = gpar(fill = "#2c3384", col = "#2c3384"))
        popViewport()

        ##  Report Text
        # pushViewport(custom.isr$Grid_Objects$report_text.vp)
        # grid.raster(custom.isr$Report_Text_SPANISH)
        # popViewport()
        pushViewport(custom.isr$Grid_Objects$report_text.vp)
        # grid.draw(splitTextGrob(custom.isr$Report_Text_SPANISH, gp=gpar(fontfamily="Helvetica-Narrow", col="black", cex=0.75), hjust="top", vjust="left", default.units="native"))
        grid.text(
            x = 0.01, y = 0.5, # x = 0.015,
            label = custom.isr$Report_Text_SPANISH,
            gp = gpar(fontfamily = "Helvetica-Narrow", col = "black", cex = 0.825),
            just = "left", default.units = "native"
        )
        popViewport()

        ##  Color block 2
        pushViewport(custom.isr$Grid_Objects$color_block_2.vp)
        grid.rect(gp = gpar(fill = "#2c3384", col = "#2c3384"))
        popViewport()

        pushViewport(custom.isr$Grid_Objects$report.student.name.vp)
        grid.rect(gp = gpar(fill = "#235e39", col = "#235e39"))
        grid.text(
            x = 0.05, y = 0.5, paste(FIRST_NAME, " ", LAST_NAME, sep = ""),
            gp = gpar(fontface = "bold", fontfamily = "Helvetica-Narrow", col = "white", cex = 1.25), just = "left", default.units = "native"
        )
        popViewport()
        # pushViewport(custom.isr$Grid_Objects$report.school.name.vp)
        # grid.rect(gp=gpar(fill="#339933", col="#339933"))
        # grid.text(x= 0.125, y=0.5, tmp_school_name, gp=gpar(fontface="bold", fontfamily="Helvetica-Narrow", col="white", cex=1.25), just="left", default.units="native")
        # popViewport()

        # pushViewport(custom.isr$Grid_Objects$left.border.vp)
        # grid.rect(gp = gpar(fill = "#2c3384", col = "#2c3384"))
        # grid.text("2024 Reporte del Crecimiento Estudiantil en el Dominio del Idioma Ingl\u{E9}s",
        #     gp = gpar(fontface = "bold", fontfamily = "Helvetica-Narrow", col = "white", cex = 2), rot = 270, just = "center", default.units = "native"
        # )
        # popViewport()

        ## Bottom Legend
        pushViewport(custom.isr$Grid_Objects$bottom.border.vp)
        # grid.text(x=0.02, y=0.65, "Para obtener m\u{E1}s informaci\u{F3}n visite www.cde.state.co.us/accountability/englishlanguageproficiencygrowth o llamar El Departamento de",
        # gp=gpar(cex=0.75, col="black", fontfamily="Helvetica-Narrow"), default.units="native", just=c("left", "top"))
        copyright.text <- "Producido por El Departamento de Educaci\u{F3}n de Colorado (CDE) y El Centro de Evaluaci\u{F3}n, Inc. Distribuido por CDE."
        grid.text(
            x = 0.02, y = 0.50, copyright.text, # y=0.30
            gp = gpar(cex = 0.825, col = "black", fontfamily = "Helvetica-Narrow"), default.units = "native", just = c("left", "top")
        )
        popViewport()

        dev.off()
    }
    