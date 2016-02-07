use algebra::Operator;
use common::dataset::DataSet;
use common::err::Result;
use common::session::Session;
use common::types::Ty;
use common::plugin::{PluginManager, TypeRegistry};
use engine::{LocalQueryRunner, QueryRunner};

use sql::SQLPackage;

use df::DataFrame;

pub struct TokamakContext {
  pub session: Session,
  executor: Box<QueryRunner>,
}

impl TokamakContext {
  pub fn new() -> Result<TokamakContext> {
    let mut executor = Box::new(LocalQueryRunner::new());

    try!(executor.add_plugin(Box::new(SQLPackage)));

    Ok(TokamakContext {
      session: executor.default_session(),
      executor: executor,
    })
  }

  pub fn runner(&self) -> &QueryRunner {
    &*self.executor
  }

  pub fn plugin_manager(&self) -> &PluginManager {
    self.executor.plugin_manager()
  }

  #[inline]
  pub fn get_type(&self, type_sign: &str) -> Result<Ty> {
    self.plugin_manager().ty_registry().get(type_sign)
  }

  #[inline]
  pub fn all_types(&self) -> Vec<&str> {
    self.plugin_manager().ty_registry().all()
  }

  pub fn from(&self, ds: DataSet) -> DataFrame {
    DataFrame {
      ctx: self,
      plan: Operator::Scan(ds),
    }
  }

  pub fn random_table(&self, type_strs: Vec<&str>, rownum: usize) -> DataFrame {
    let types = types_str_to_types(self.executor.plugin_manager().ty_registry(), &type_strs);
    DataFrame {
      ctx: self,
      plan: Operator::Scan(DataSet::random(types, rownum)),
    }
  }
}

pub fn types_str_to_types(type_registry: &TypeRegistry, type_strs: &Vec<&str>) -> Vec<Ty> {
  type_strs.iter().map(|s| type_registry.get(s).ok().unwrap()).collect::<Vec<Ty>>()
}
