use common::rows::{
	BorrowedPage,
	MiniPage,
	Page, 
	PosId
};

use super::Schema;

pub struct Projector
{
	pos_list: Vec<PosId> 
}
//
//impl Projector
//{
//	pub fn new(schema: &Schema, Vec<&Expr>)
//	{
//		Projector
//	}
//}

impl Projector
{
	pub fn next<'a>(&self, page: &'a Page, writer: &mut BorrowedPage<'a>)
	{
		let projected = self.pos_list.iter()
			.map(|id| page.minipage(*id))
			.collect::<Vec<&MiniPage>>();
		
		writer.set(projected);
	}
}

#[cfg(test)]
mod tests {
	use common::rows::{
		Page,
		OwnedPageBuilder,
		PageId
	};
	use common::plugin::*;
	use common::session::Session;
	use common::storage::{RandomTable, MemTable};
	use common::types::*;
	
	use plan::expr::*;
	use driver::DriverContext;
	
	use super::*;
	
	#[test]
	pub fn tpch_q1() {
		let plugin_mgr = PluginManager::new();
		
		let tb_types: Vec<Ty> = vec![
	    i64_ty(), // l_orderkey      bigint
	    i64_ty(), // l_partkey       bigint
	    i64_ty(), // l_suppkey       bigint
	    i32_ty(), // l_linenumber    int
	    f64_ty(), // l_quantity      double,
	    f64_ty(), // l_extendedprice double,
	    f64_ty(), // l_discount      double,
	    f64_ty(), // l_tax           double,
	    // string types are not implmeneted yet.	    
	  ];
	  
	  let tb_field_names: Vec<&str> = vec![
	  	"l_orderkey",
	  	"l_partkey",
	  	"l_suppkey",
	  	"l_linenumber",
	  	"l_quantity",
	  	"l_extendedprice",
	  	"l_discount",
	  	"l_tax"
	  ];
		
		let l_orderkey = Field(&f64_ty(), "l_orderkey");
		let l_quantity = Field(&f64_ty(), "l_quantity");	  
	  
	  let exprs = vec![
	  		Box::new(l_orderkey),
				Box::new(l_quantity)
  	];
  	
  	let schema = (&tb_types, &tb_field_names);
  	let session    = Session;
  			
		let mut input  = RandomTable::new(&session, &tb_types, 1024);
		let output_tys = exprs.iter()
											.map(|e| e.ty().clone())
											.collect::<Vec<Ty>>();
		let mut output = MemTable::new(&session, &output_tys, &vec!["x"]);
		
		//let projector = Projector::new(&schema, &exprs).ok().unwrap();
		let mut builder = OwnedPageBuilder::new(&output_tys);
		
		
		loop { 
		  let mut read_page = input.next().unwrap();
		  
		  if read_page.value_count() == 0 {
		  	break;
		  }
		  
		  //processor.process(read_page, &mut builder);
		  //output.write(builder.build(read_page.value_count()));
		  //builder.reset();
		}
		
		assert_eq!(1, output.col_num());
		assert_eq!(1024, output.row_num());
		
		for x in output.reader() {
			let r: (f64) = x.ok().unwrap();
			println!("{}", r);
		}
	}
}