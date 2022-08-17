import sys
import json 

JSON_PATH = "/home/michelle/Documents/sa-for-wasm/wasabi/lib/wasm/tests/callgraph-eval/data/data.json"

def main():
    args = sys.argv[1:]
    if len(args) > 0:
        print("Usage: analysis.py")
        print("Compute precision, recall for each library and coverage for each library test case")
        sys.exit()

    data = json.load(open(JSON_PATH))
    
    # for each tool precision = |Mdyn \cap Mstat| / |Mstat|
    # for each tool recall    = |Mdyn \cap Mstat| / |Mdyn|
    # coverage for each test_case = {
    #   exported_covered = (|dyn_exports| / |total_exports|)*100
    #   funcs_convered = (|dyn_funcs| / |total_funcs|)*100
    # } 
    
    for lib in data["library_data"]:
        
        M_dyn  = set(lib["dyn_total_reachable_functions"]["names"])
        for tool in lib["tools"]:            
            if tool["reachable_functions"]["names"] == "DNE":
                tool["precision"] = "DNE"
                tool["recall"] = "DNE"
            else:
                M_stat_ = tool["reachable_functions"]["names"]
                M_stat = set()
                export_names = {item['name']:item['internal_id'] for item in lib['static_info']['exports']['names'] if item['type'] == 'function'}
                import_names = {item["module_name"]+"."+item["export_name_within_module"]: item['internal_name'] for item in lib['static_info']['imports']['names'] if item['type'] == "function"}
                for f in M_stat_:
                    if not str(f).isdigit(): #it is an exported or imported function index and has to be replaced with its internal id 
                        if f in export_names.keys(): M_stat.add(int(export_names[f]))
                        elif f in import_names.keys(): M_stat.add(int(import_names[f]))
                        else: sys.exit("Unknown string found {}".format(f))
                    else: M_stat.add(f)
                
                cap_set = M_stat.intersection(M_dyn)            
                
                recall, precision = len(cap_set)/len(M_dyn), len(cap_set)/len(M_stat)            
                tool["reachable_functions"]["names"] = list(M_stat)
                tool["precision"] = precision
                tool["recall"] = recall

        M_stat_exports = lib["static_info"]["exports"]["count_exported_funcs"]
        M_stat_funcs = lib["static_info"]["count_functions"]
        for test in lib["tests"]:
            M_dyn_exports = test["dyn_reachable_exports"]["count"]
            M_dyn_funcs =  test["dyn_reachable_functions"]["count"]
            percent_exports = (M_dyn_exports/M_stat_exports)*100
            percent_funcs = (M_dyn_funcs/M_stat_funcs)*100
            test["coverage"] = {
                "exports_covered": percent_exports,
                "funcs_covered": percent_funcs
            } #TODO instructions covered 

    json.dump(data, open(JSON_PATH, 'w'), indent=2)

if __name__ == "__main__":
    main()
