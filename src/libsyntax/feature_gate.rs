#[derive(Clone, Copy)]
pub enum UnstableFeatures {
    /// Hard errors for unstable features are active, as on
    /// beta/stable channels.
    Disallow,
    /// Allow features to me activated, as on nightly.
    Allow,
}