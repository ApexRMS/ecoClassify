use clap::Parser;
use rayon::prelude::*;
mod raster_functions;
use raster_functions::*;


#[derive(Parser)]
#[command(name = "geotiff-decomposer")]
struct Args {
    #[arg(short, long)]
    input: String,

    #[arg(short, long)]
    output: String,

    #[arg(long, default_value = "3")]
    pca_components: usize,

    #[arg(long, default_value = "3")]
    texture_kernel: usize,

    #[arg(long, default_value = "4")]
    jobs: usize,
}



fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    rayon::ThreadPoolBuilder::new().num_threads(args.jobs).build_global()?;

    // Load and normalize bands
    let bands = read_bands(&args.input)?;
    let normalized: Vec<_> = bands.iter()
        .map(|b| normalize_band(b))
        .collect();

    // Stack to tabular
    let base_table = stack_bands(&normalized);

    // PCA
    let pca_table = compute_pca(&base_table, args.pca_components)?;

    // Texture feature (mean)
    let texture_means: Vec<_> = normalized
        .par_iter()
        .map(|b| local_mean(b, args.texture_kernel))
        .collect();
    let texture_table = stack_bands(&texture_means);

    // Concatenate all tables
    let final_table = ndarray::concatenate![ndarray::Axis(1), base_table, pca_table, texture_table];

    // Save
    write_csv(&final_table, &args.output)?;
    Ok(())
}
