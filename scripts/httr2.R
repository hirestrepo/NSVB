library(httr2)

req <- request(example_url())

req |> req_dry_run()

req |> req_headers() |> req_dry_run()

req |> 
  req_body_json(list(x = 1, y = "a")) |>
  req_dry_run()


req <- request(example_url()) |> req_url_path("/json")
resp <- req |> req_perform()
resp |> resp_raw()

resp  |> resp_status()





argList <- list(snum= 2, 
                wc = 282021,
                estOnly = "Y", 
                outputFormat='NJSON')

req <- request("https://apps.fs.usda.gov/fiadb-api/fullreport")

req |> req_dry_run()

req |> req_headers() |> req_dry_run()

req |> 
  req_body_json(argList) |>
  req_dry_run()



resp <- req |> 
  req_body_json(argList) |>
  req_perform()
