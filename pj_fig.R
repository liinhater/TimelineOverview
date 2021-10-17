pj.timeline.fig <- function(ui.excel, ui.highlight.task) {
  
  #---- Invoke data ---- 
  ds.excel <- ui.excel %>%
    mutate(start_dt = as.Date(start_dt), end_dt = as.Date(end_dt), 
           timepoint = as.Date(timepoint), task_sdt = as.Date(start_dt))
  
  proj.list <- ds.excel %>%
    distinct(project) %>%
    mutate(pj_ord = n():1) 
  
  task.list <- ds.excel %>%
    distinct(project, task, start_dt) %>%
    arrange(start_dt) %>%
    group_by(project) %>%
    mutate(task_num = row_number())
  
  note.wrap <- ds.excel %>%
    select(project, task, note) %>%
    group_by(project, task, note) %>%
    mutate(note_wrap = paste(strwrap(note, 8), collapse = "\n"))
  
  
  ds <- left_join(left_join(left_join(ds.excel, proj.list), task.list), note.wrap) %>%
    filter(!is.na(task)) %>%
    mutate(pj.y = pj_ord + 0.2*(task_num - 1)) %>%
    gather(key = "date_type", value = "yr_mon", 
           -project, -task, -note, -timepoint, -pj_ord, -task_num, -task_sdt, -pj.y, -note_wrap)
  
  
  #---- Create date list used to x-axis ----
  mon.range <- data.frame(
    "month_list" = seq(min(ds$yr_mon), max(ds$yr_mon), by = "month"),
    "month_list_ft" = format(seq(min(ds$yr_mon), max(ds$yr_mon), by = "month"), "%b")
  )
  
  if ( all(is.na(grep("\\d{4}-01-01", mon.range[,1]))) ) {
    mon.anno <- as.Date(format(mon.range[1,1], "%Y-%m-01"))
  } else {
    mon.anno <- mon.range[grep("\\d{4}-01-01", mon.range[,1]),1]  
  }
  
  #---- Create plot ----
  if (is.null(ui.highlight.task)) {
    pj.timeline <- 
      ggplot(ds, aes(x = pj.y, y = timepoint, group = task, 
                     text = paste("Task: ", note, "\nDate: ", timepoint))) +
      geom_rect(
        aes(
          xmin = 0, xmax = max(pj.y), 
          ymin = as.Date(format(ymd(Sys.Date()) %m+% months(-1), "%Y-%m-01")), 
          ymax = as.Date(format(ymd(Sys.Date()) %m+% months(2), "%Y-%m-01")) - 1
        ), 
        fill = "lightblue1") +
      geom_point() +
      geom_line(aes(x = pj.y, y = yr_mon, group = interaction(project, task))) +
      geom_text(aes(x = pj.y, y = task_sdt - 10, group = project, 
                    label = task),
                nudge_x = 0.08) + 
      scale_y_date(date_breaks = "1 month", date_labels = "%b") +
      scale_x_continuous(breaks = ds$pj_ord, labels = ds$project) +
      ylab(" ") +
      xlab(NULL) +
      coord_flip() 
    
    ggplotly(pj.timeline, tooltip = "text") %>%
      layout(hovermode = "x") %>%
      add_annotations(
        height = 190,
        x = as.numeric(mon.anno),
        y = rep(-0.25, length(mon.anno)),
        xref = "x",
        yref = "paper",
        text = format(mon.anno, "%Y"),
        showarrow = F
      ) 
        
  } else {
    
    ds.highlight <- ds[grep(paste(ui.highlight.task, collapse = "|"), ds$note),]
    myColors <- brewer.pal(max(3, length(unique(ds.highlight$note))), "Set1")
    names(myColors) <- levels(ds.highlight$note)
    
    pj.timeline <- 
      ggplot(ds, aes(x = pj.y, y = timepoint, group = task, 
                     text = paste("Task: ", note, "\nDate: ", timepoint))) +
      geom_rect(
        aes(
          xmin = 0, xmax = max(pj.y), 
          ymin = as.Date(format(ymd(Sys.Date()) %m+% months(-1), "%Y-%m-01")), 
          ymax = as.Date(format(ymd(Sys.Date()) %m+% months(2), "%Y-%m-01")) - 1
        ), 
        fill = "lightblue1") +
      geom_point() +
      geom_line(aes(x = pj.y, y = yr_mon, group = interaction(project, task))) +
      geom_text(aes(x = pj.y, y = task_sdt - 10, group = project, 
                    label = task),
                nudge_x = 0.08) + 
      geom_point(ds.highlight, mapping = aes(color = note)) + 
      scale_color_manual(name = " ", values = myColors) +
      scale_y_date(date_breaks = "1 month", date_labels = "%b") +
      scale_x_continuous(breaks = ds$pj_ord, labels = ds$project) +
      ylab(" ") +
      xlab(NULL) +
      coord_flip()     
    
    ggplotly(pj.timeline, tooltip = "text") %>%
      layout(hovermode = "x") %>%
      add_annotations(
        height = 190,
        x = as.numeric(mon.anno),
        y = rep(-0.25, length(mon.anno)),
        xref = "x",
        yref = "paper",
        text = format(mon.anno, "%Y"),
        showarrow = F
      ) %>%
      layout(legend = list(title = list(text = "Highlighted Note(s)")))
  }

}


