
# MARRS_ACOUSTICS Repository

**Author:** Ben Williams, University College London (UCL)

This repository is a modified clone of [the Google Perch repository](https://github.com/google-research/perch) at commit [12bee2980e81c6a95a5e48214000d1f335274ea1](https://github.com/google-research/perch/commit/12bee2980e81c6a95a5e48214000d1f335274ea1) (1 August 2024). Please see the license of this repository. This repostitory has been adapted to analyse a global dataset of coral reef sound recordings, and may not reflect updates in the official Perch repository after this date.

### Workflow

The workflow in this repository can be used to rapidly detect new reef sounds input by the user. This includes two key steps:

1. **Data Embedding**: Run `reefs_embed.py` to embed large coral reef audio datasets.
2. **Classifier Training and Inference**: Use `reefs_agile.ipynb` to rapidly train classifiers for new sounds and perform inference.

### Set Up

This guide explains how to set up Perch with GPU usage on Linux. The system used for this guide is a Dell XPS 15 with an Nvidia RTX 4060 GPU, running Ubuntu 24.04.

#### Step 2: Clone this Repository

Navigate to the local directory you wish to work from and clone this repository from GitHub.

```bash
git clone https://github.com/BenUCL/MARRS_global_acoustic_study.git
```

#### Step 2: Create the Conda Environment

Use the `perch_conda_env.yml` file to create the Conda environment. This will install TensorFlow 2.15.0 alongside the correct CUDA libraries. Note different TensorFlow versions can be a big source of pain as these may conflict with other packages in perch, these steps ensure the right version is installed.

```bash
cd /marrs_acoustics/code/setup/env_setup
conda env create -f perch_conda_env.yml
```

#### Step 3: Verify TensorFlow Installation and GPU Detection

Activate the Conda environment and run a test script to check if TensorFlow is installed correctly and if the GPU is recognized. The script will print out the number of GPUs found, the TensorFlow version and run a test model.

```bash
conda activate perch_conda_env
python tf_test.py
```

If no GPU is found, follow the advice [here](https://github.com/tensorflow/tensorflow/issues/63362#issuecomment-2016019354) and run these lines:

```bash
export NVIDIA_DIR=$(dirname $(dirname $(python -c "import nvidia.cudnn;print(nvidia.cudnn.__file__)")))
export LD_LIBRARY_PATH=$(echo ${NVIDIA_DIR}/*/lib/ | sed -r 's/\s+/:/g')${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}
```

Rerun the test script to ensure the GPU is now recognized by TensorFlow:

```bash
python tf_test.py
```

#### Step 4: Install the Poetry Environment

Next, install the Poetry environment for Perch. Make sure you are in the activated perch_conda_env environment. This will add additional packages required by Perch to this conda environment. Note that TensorFlow has been removed from the `pyproject.toml` file used by Perch to avoid updating to later versions which cause conflicts.

```bash
cd /marrs_acoustics/code
poetry install
```

Now update the lock file, this can take several minutes to resolve all the dependancies. Then we run install, this will use poetry to install all the remaining dependancies in the conda env.

```bash
poetry lock
poetry install
```

#### Step 5: Verify Installation

The Conda environment should now have the `chirp` package and other dependencies installed. To verify the installation, check the version of `chirp`:

```bash
python -c "import chirp; print(chirp.__version__)"
```

Finally, verify the TensorFlow version and GPU detection again. Note that TensorFlow may have updated to version 2.16.2 which is fine:

```bash
python tf_test.py
```

If the test script shows that 1 or more GPUs are found, the setup is successful and GPU support is enabled.